{-# LANGUAGE OverloadedStrings #-}

module Arrow (upDownArrows, testUpDownArrows) where

import Data.List (nub)
import qualified Data.Set as S (empty, insert, member, Set)
import qualified Control.Monad.State as ST (evalState, get, modify, State)
import qualified Data.Text as T (head, tail, Text)

import Text.Pandoc.Definition (nullMeta, Attr, Inline(Link, Span, Str),
                               Block(BlockQuote, BulletList, CodeBlock, DefinitionList, Div, Header, LineBlock, OrderedList, Para),
                               Pandoc(..))
import Text.Pandoc.Walk (walkM)

import Utils (isUniqueAll)

-- Compile-time Layout Optimization: annotate self-reference links with whether they are 'before' or 'after' the link, so they are decorated with either '↑' or '↓' icons to help the reader know what the link refers to & if they have read it already.
-- Doing this at compile-time reduces the burden on client-side JS & layout shift.

-- `upDownArrows` provides a Pandoc walk function which can be called from inside Hakyll on an AST.
-- NOTE: this must be called after all rewrites which will add IDs to the AST, such as `addID` which adds identifiers to Links based on the metadata database.

-- We do this by walking the Pandoc AST in order, and keeping track of whether a referenced ID has been before; if it has, then it must be 'before', and if it has not, then it must be 'after'.
-- Specifically:
-- initialize an empty `Set` using the State monad; use a monadic `walkM` (depth-first pre-order traversal?); for each element with an ID, add that ID to the set; if the element is a Link which has a `#` prefix on its target, then look up the target in the Set; if it is present, then the target is 'before' and one adds .arrow-up; if it is not present, then the target must be 'after' and it gets .arrow-down. IDs are required by HTML to be unique, so the first instance must be the only instance and we do not need to worry about multiple definitions and 'which' is targeted.

type Refs = S.Set T.Text

arrowUp, arrowDown :: T.Text
arrowUp = "arrow-up"
arrowDown = "arrow-down"

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
-- WARNING: `Table` is skipped due to the sheer complexity of Table processing; anyone who needs to link to a Table should wrap it in a Div & link that instead.
addArrowClass x = return x  -- wildcard: ignore all other blocks

addArrowClassInline :: Inline -> ST.State Refs Inline
addArrowClassInline link@(Link attr@(_,classes,_) inlines (url, title))
  | T.head url /= '#' = return link
  | otherwise = do
      refs <- ST.get
      let newAttr = if S.member (T.tail url) refs &&
                       -- WARNING: depending on the recursion pattern, it's possible to add *both*! We must check to avoid that:
                       arrowUp `notElem` classes && arrowDown `notElem` classes
                    then addClass arrowUp attr
                    else addClass arrowDown attr
      return $ Link newAttr inlines (url, title)
addArrowClassInline (Span attr inlines) = do
  addID attr
  inlines' <- mapM addArrowClassInline inlines
  return $ Span attr inlines'
addArrowClassInline x = return x  -- other inlines

addClass :: T.Text -> Attr -> Attr
-- NOTE: we use nub because the class list is guaranteed to never be more than 3 or 4 long & Data.Set is overkill & more verbose.
addClass cls (i, classes, kv) = (i, nub (cls:classes), kv)

upDownArrows :: Pandoc -> Pandoc
upDownArrows (Pandoc meta blocks) = Pandoc meta (ST.evalState (walkM addArrowClass blocks) S.empty)

testUpDownArrows :: [(Pandoc, Pandoc)]
testUpDownArrows = filter (uncurry ((/=) . upDownArrows)) $ map (\(a,b) -> (Pandoc nullMeta a, Pandoc nullMeta b)) testCases
  where
    testCases :: [([Block], [Block])]
    testCases = isUniqueAll
      [([Para [Link ("", [], []) [Str "simpleCase"] ("#target", "")]],
        [Para [Link ("", [arrowDown], []) [Str "simpleCase"] ("#target", "")]])
      , ([Para [Link ("", [], []) [Str "sameBlockCase"] ("#target", ""), Span ("target", [], []) [Str "span"]]],
         [Para [Link ("", [arrowDown], []) [Str "sameBlockCase"] ("#target", ""), Span ("target", [], []) [Str "span"]]])
      , ([Para [Link ("", [], []) [Str "differentBlockCase"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]],
         [Para [Link ("", [arrowDown], []) [Str "differentBlockCase"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]])
      , ([Para [Link ("", [], []) [Str "nestedCase"] ("#target", "")], Div ("", [], []) [Para [Span ("target", [], []) [Str "span"]]]],
          [Para [Link ("", [arrowDown], []) [Str "nestedCase"] ("#target", "")], Div ("", [], []) [Para [Span ("target", [], []) [Str "span"]]]])
      , ([Para [Link ("", [], []) [Str "headerCase"] ("#target", "")], Header 1 ("target", [], []) [Str "header"]],
         [Para [Link ("", [arrowDown], []) [Str "headerCase"] ("#target", "")], Header 1 ("target", [], []) [Str "header"]])
      , ([Para [Span ("target", [], []) [Str "span"]], Para [Link ("", [], []) [Str "beforeLinkCase"] ("#target", "")]],
         [Para [Span ("target", [], []) [Str "span"]], Para [Link ("", [arrowUp], []) [Str "beforeLinkCase"] ("#target", "")]])
      , ([Para [Link ("", [], []) [Str "multipleLinksCase"] ("#target", ""), Link ("", [], []) [Str "link2"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]],
          [Para [Link ("", [arrowDown], []) [Str "multipleLinksCase"] ("#target", ""), Link ("", [arrowDown], []) [Str "link2"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]])
      , ([Para [Link ("", [], []) [Str "multipleTargetsCase"] ("#target1", ""), Link ("", [], []) [Str "link2"] ("#target2", "")], Para [Span ("target1", [], []) [Str "span1"], Span ("target2", [], []) [Str "span2"]]],
         [Para [Link ("", [arrowDown], []) [Str "multipleTargetsCase"] ("#target1", ""), Link ("", [arrowDown], []) [Str "link2"] ("#target2", "")], Para [Span ("target1", [], []) [Str "span1"], Span ("target2", [], []) [Str "span2"]]])
      , ([Para [Link ("", [], []) [Str "nonExistentTargetCase"] ("#nonexistent", "")]],
         [Para [Link ("", [arrowDown], []) [Str "nonExistentTargetCase"] ("#nonexistent", "")]])
      , ([Para [Str "noLinksCase: no links or targets here"]],
         [Para [Str "noLinksCase: no links or targets here"]])
      , ([Div ("", [], []) [Para [Span ("target", [], []) [Str "span"]]], Para [Link ("", [], []) [Str "beforeLinkNestedCase"] ("#target", "")]],
         [Div ("", [], []) [Para [Span ("target", [], []) [Str "span"]]], Para [Link ("", [arrowUp], []) [Str "beforeLinkNestedCase"] ("#target", "")]])
      , ([Header 1 ("target", [], []) [Str "header"], Para [Link ("", [], []) [Str "beforeLinkHeaderCase"] ("#target", "")]],
         [Header 1 ("target", [], []) [Str "header"], Para [Link ("", [arrowUp], []) [Str "beforeLinkHeaderCase"] ("#target", "")]])
      , ([Para [Span ("target1", [], []) [Str "span1"], Span ("target2", [], []) [Str "span2"]], Para [Link ("", [], []) [Str "multipleTargetsWithBeforeLinksCase"] ("#target1", ""), Link ("", [], []) [Str "link2"] ("#target2", "")]],
         [Para [Span ("target1", [], []) [Str "span1"], Span ("target2", [], []) [Str "span2"]], Para [Link ("", [arrowUp], []) [Str "multipleTargetsWithBeforeLinksCase"] ("#target1", ""), Link ("", [arrowUp], []) [Str "link2"] ("#target2", "")]])
      , ([Para [Span ("target1", [], []) [Str "span1"]], Para [Link ("", [], []) [Str "beforeAfterMixedCase"] ("#target1", ""), Link ("", [], []) [Str "link2"] ("#target2", "")], Para [Span ("target2", [], []) [Str "span2"]]],
         [Para [Span ("target1", [], []) [Str "span1"]], Para [Link ("", [arrowUp], []) [Str "beforeAfterMixedCase"] ("#target1", ""), Link ("", [arrowDown], []) [Str "link2"] ("#target2", "")], Para [Span ("target2", [], []) [Str "span2"]]])
      , ([Para [Link ("", [], []) [Str "mixedLinkOrderCase"] ("#target", ""), Link ("", [], []) [Str "link2"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]], Para [Link ("", [], []) [Str "link3"] ("#target", "")]],
         [Para [Link ("", [arrowDown], []) [Str "mixedLinkOrderCase"] ("#target", ""), Link ("", [arrowDown], []) [Str "link2"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]], Para [Link ("", [arrowUp], []) [Str "link3"] ("#target", "")]])
      , ([Div ("", [], []) [Para [Link ("", [], []) [Str "sameDivBlockCase"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]]],
         [Div ("", [], []) [Para [Link ("", [arrowDown], []) [Str "sameDivBlockCase"] ("#target", "")], Para [Span ("target", [], []) [Str "span"]]]])
      ]
