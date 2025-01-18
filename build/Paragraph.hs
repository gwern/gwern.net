{-# LANGUAGE OverloadedStrings #-}
module Paragraph where

import Control.Concurrent (forkIO)
import Control.Monad (unless, void)
import Data.List (isInfixOf, intercalate)
import qualified Data.Map as M (member)
import qualified Data.Text as T (breakOnAll, pack, unpack, Text)
import Data.FileStore.Utils (runShellCommand)
import Text.Show.Pretty (ppShow)
import Text.Pandoc (def, pandocExtensions, readerExtensions, readMarkdown, runPure, writeHtml5String, Pandoc)
import System.Exit (ExitCode(ExitFailure))
import qualified Data.ByteString.Lazy.UTF8 as U (toString)

import GTX (readGTXFast)
import Metadata.Format (cleanAbstractsHTML)
import LinkMetadataTypes (Metadata)
import Utils (replace, printGreen, trim, toMarkdown, printRed, safeHtmlWriterOptions, anyInfix, isLocal, delete)
import Config.Paragraph as C
import Config.Misc as CD (cd)

import Query (extractURLs)

-- If a String (which is not HTML!) is a single long paragraph (has no double-linebreaks), call out to paragraphizer.py, which will use GPT-4o to try to break it up into multiple more-readable paragraphs.
-- This is quite tricky to use: it wants non-HTML plain text (any HTML will break GPT-4o), but everything else wants HTML
processParagraphizer :: Metadata -> FilePath -> String -> IO String
processParagraphizer _ _ "" = return ""
processParagraphizer md p a = -- the path is necessary to check against the whitelist
      if length a < C.minLength || paragraphized p a then return a
      else do let a' = delete "<p>" $ delete "</p>" a
              let a'' = trim $ replace "\160" " " $ toMarkdown a'
              CD.cd
              (status,_,mb) <- runShellCommand "./" Nothing "python3" ["static/build/paragraphizer.py", a'']
              case status of
                ExitFailure err -> printGreen (intercalate " : " [a, a', ppShow status, ppShow err, ppShow mb]) >> printRed "Paragraphizer failed!" >> return a
                _ -> do let a''' = T.pack $ trim $ U.toString mb
                        if a''' == "" then return a else
                         do let clean = runPure $ do
                                  pandoc <- readMarkdown def{readerExtensions=pandocExtensions} a'''
                                  html <- writeHtml5String safeHtmlWriterOptions pandoc
                                  return (T.unpack html, pandoc)
                            case clean of
                              Left e -> error $ ppShow e ++ " : " ++ a ++ " : " ++ a'' ++ " : " ++ T.unpack a'''
                              Right (output,document) -> do checkURLs md document
                                                            return (cleanAbstractsHTML output)

-- EXPERIMENTAL: the GPT-4o paragraphizer seems to confabulate a fair number of wrong URLs; let's double-check them manually for a while to see how bad the problem is.
checkURLs :: Metadata -> Pandoc -> IO ()
checkURLs md p = let urls = filter (\u -> not (isLocal u || M.member (T.unpack u) md)) $ extractURLs p in
                   mapM_ (\u -> forkIO $ void $ runShellCommand "./" (Just [("DISPLAY", ":0")]) "chromium" [T.unpack u]) urls

-- Is an annotation (HTML or Markdown) already If the input has more than one <p>, or if there is one or more double-newlines, that means this input is already multiple-paragraphs
-- and we will skip trying to break it up further.
paragraphized :: FilePath -> String -> Bool
paragraphized f a = f `elem` C.whitelist ||
                  paragraphsMarkdown a || blockElements a || length (paragraphsHtml a) > 1
 where
   -- double newlines are only in Markdown strings, and split paragraphs:
   paragraphsMarkdown :: String -> Bool
   paragraphsMarkdown b = "\n\n" `isInfixOf` b
   blockElements :: String -> Bool
   -- full-blown lists or blockquotes also imply it's fully-formatted
   blockElements b = anyInfix b ["<ul>", "<ol>", "<ul type=", "<ol type=", "<blockquote>", "<figure>", "<table>", "<div class=", "<br />"]
   -- annotations are wrapped in a '<p>...</p>' pair, unless they start with another block element; if there are two or more '<p>', then, there are at least two paragraphs (because it must be '<p>...</p> ... <p>...</p>') and it counts as being paragraphized.
   paragraphsHtml :: String -> [(T.Text,T.Text)]
   paragraphsHtml b = T.breakOnAll "<p>" (T.pack b)


-- read a GTX database and look for annotations that need to be paragraphized.
warnParagraphizeGTX :: FilePath -> IO ()
warnParagraphizeGTX path = do CD.cd
                              gtx <- readGTXFast path
                              let unparagraphized = filter (\(f,(_,_,_,_,_,_,abst)) -> abst /= "" && not (paragraphized f abst)) gtx
                              unless (null unparagraphized) $ printGreen $ ppShow (map fst unparagraphized)
