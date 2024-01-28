{-# LANGUAGE OverloadedStrings #-}

module Arrow (upDownArrows, testUpDownArrows) where

import qualified Data.Set as S (empty, insert, member, Set)
import qualified Control.Monad.State as ST (evalState, get, modify, State)
import qualified Data.Text as T (head, tail, Text)

import Text.Pandoc.Definition (nullMeta, Attr, Inline(Link, Span),
                               Block(BlockQuote, BulletList, CodeBlock, DefinitionList, Div, Header, LineBlock, OrderedList, Para, Table),
                               Pandoc(..), TableBody(..), Row(..), Cell(..))
import Text.Pandoc.Walk (walkM)

import Config.Misc as C (arrowTestCases, arrowUp, arrowDown)
import LinkIcon (addIcon)

-- Compile-time Layout Optimization: annotate self-reference links with whether they are 'before' or 'after' the link, so they are decorated with either '↑' or '↓' icons to help the reader know what the link refers to & if they have read it already.
-- Doing this at compile-time reduces the burden on client-side JS & layout shift.

-- `upDownArrows` provides a Pandoc walk function which can be called from inside Hakyll on an AST.
-- NOTE: this must be called after all rewrites which will add IDs to the AST, such as `addID` which adds identifiers to Links based on the metadata database.

-- We do this by walking the Pandoc AST in order, and keeping track of whether a referenced ID has been before; if it has, then it must be 'before', and if it has not, then it must be 'after'.
-- Specifically:
-- initialize an empty `Set` using the State monad; use a monadic `walkM` (depth-first pre-order traversal?); for each element with an ID, add that ID to the set; if the element is a Link which has a `#` prefix on its target, then look up the target in the Set; if it is present, then the target is 'before' and one adds 'arrow-up' link-icon; if it is not present, then the target must be 'after' and it gets 'arrow-down' link-icon. IDs are required by HTML to be unique, so the first instance must be the only instance and we do not need to worry about multiple definitions and 'which' is targeted.

type Refs = S.Set T.Text

addID :: Attr -> ST.State Refs ()
addID ("",_,_) = return ()
addID (i, _,_) = ST.modify $ S.insert i

addArrowClass :: Block -> ST.State Refs Block
addArrowClass (Para inlines) = Para <$> mapM addArrowClassInline inlines
addArrowClass (Header level attr inlines) = do
  addID attr
  inlines' <- mapM addArrowClassInline inlines
  return $ Header level attr inlines'
addArrowClass (Div attr blocks) = do
  addID attr
  blocks' <- mapM addArrowClass blocks
  return $ Div attr blocks'
addArrowClass (BlockQuote blocks) = do
  blocks' <- mapM addArrowClass blocks
  return $ BlockQuote blocks'
addArrowClass x@(CodeBlock attr _) = do
  addID attr
  return x
addArrowClass (LineBlock lns) = do
  lns' <- mapM (mapM addArrowClassInline) lns
  return $ LineBlock lns'
addArrowClass (OrderedList listAttr listItems) = do
  listItems' <- mapM (mapM addArrowClass) listItems
  return $ OrderedList listAttr listItems'
addArrowClass (BulletList listItems) = do
  listItems' <- mapM (mapM addArrowClass) listItems
  return $ BulletList listItems'
addArrowClass (DefinitionList defs) = do
  defs' <- mapM (\(inlines, blocksList) -> do
                    inlines' <- mapM addArrowClassInline inlines
                    blocksList' <- mapM (mapM addArrowClass) blocksList
                    return (inlines', blocksList')) defs
  return $ DefinitionList defs'
-- WARNING: `Table` is only partially processed due to the sheer complexity of Table processing; we attempt to process cells, but skip the headers/footers/captions, so... don't link stuff there, or manually specify arrow-up/down if you care.
addArrowClass (Table attr caption colSpecs tableHead tableBodies tableFoot) = do
  addID attr
  tableBodies' <- mapM addArrowClassTableBody tableBodies
  return $ Table attr caption colSpecs tableHead tableBodies' tableFoot
addArrowClass x = return x  -- wildcard: ignore all other blocks

addArrowClassTableBody :: TableBody -> ST.State Refs TableBody
addArrowClassTableBody (TableBody attr rowHeadColumns headerRows rows) = do
  addID attr
  rows' <- mapM addArrowClassRow rows
  return $ TableBody attr rowHeadColumns headerRows rows'
 where
    addArrowClassRow :: Row -> ST.State Refs Row
    addArrowClassRow (Row atr cells) = do
      addID atr
      cells' <- mapM addArrowClassCell cells
      return $ Row atr cells'
    addArrowClassCell :: Cell -> ST.State Refs Cell
    addArrowClassCell (Cell atrr alignment rowSpan colSpan blocks) = do
      addID atrr
      blocks' <- mapM addArrowClass blocks
      return $ Cell attr alignment rowSpan colSpan blocks'

addArrowClassInline :: Inline -> ST.State Refs Inline
addArrowClassInline x@(Link (_,_,kvs) _ (url, _))
  | T.head url /= '#' = return x
  | otherwise = do
      refs <- ST.get
      let keys = map fst kvs
      return $ if url == "#top" || -- interesting special-case: the ID #top is required to exist at runtime, though it is nowhere at compile-time, and it is always at the top of the page before any elements, so we must special-case it & it always is 'before' any element linking to it, so it gets arrowUp.
                       S.member (T.tail url) refs &&
                       -- WARNING: depending on the recursion pattern, it's possible to add *both*! We must check to avoid that:
                       arrowUp `notElem` keys && arrowDown `notElem` keys
                    then addIcon x (arrowUp,  "svg")
                    else addIcon x (arrowDown,"svg")
addArrowClassInline (Span attr inlines) = do
  addID attr
  inlines' <- mapM addArrowClassInline inlines
  return $ Span attr inlines'
addArrowClassInline x = return x  -- other inlines


upDownArrows :: Pandoc -> Pandoc
upDownArrows (Pandoc meta blocks) = Pandoc meta (ST.evalState (walkM addArrowClass blocks) S.empty)

testUpDownArrows :: [(Pandoc, Pandoc)]
testUpDownArrows = filter (uncurry ((/=) . upDownArrows)) $ map (\(a,b) -> (Pandoc nullMeta a, Pandoc nullMeta b)) C.arrowTestCases
