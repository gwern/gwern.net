#! /usr/bin/env runghc
{-# LANGUAGE OverloadedStrings #-}
-- example usage: $ find ~/wiki/ -name "*.md" -exec runghc markdown-length-checker.hs {} \;
--
-- default: looks for lines >110 characters long inside codeblocks, which suggests source code that
-- needs to be rewritten for more clarity.
import System.Environment (getArgs)
import Text.Pandoc (def, readMarkdown, runPure, pandocExtensions, queryWith, readerExtensions, topDown, Block(CodeBlock, Plain), Pandoc)
import qualified Data.Text as T (append, empty, length, lines, pack, unlines, Text)
import Data.Text.IO as TIO (readFile, putStr)

main :: IO ()
main = do (file:_) <- getArgs
          f <- TIO.readFile file
          let results = processLint file f
          TIO.putStr results

processLint :: FilePath -> T.Text -> T.Text
processLint f x = do let parsed = runPure $ readMarkdown def{readerExtensions = pandocExtensions } x
                     case parsed of
                      Right x' -> let matches = lineCheck x' in
                        if length matches > 0 then T.pack $ unlines $ map (\b -> f++": " ++ show b) matches else T.empty
                      Left err -> "Could not parse: " `T.append` (T.pack f) `T.append` (T.pack $ show err)

lineCheck :: Pandoc -> [Block]
lineCheck = queryWith clean . topDown longCodeLines

clean :: Block -> [Block]
clean x@(CodeBlock _ _) = [x]
clean _ = []

longCodeLines :: Block -> Block
longCodeLines x@(CodeBlock _ cntnt) = let tooLong = T.unlines $ filter (\t -> T.length t >= 110) $ T.lines cntnt
                                               in if T.length tooLong == 0 then Plain [] else x
longCodeLines _ = Plain []
