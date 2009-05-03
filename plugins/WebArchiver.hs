-- | Scans page of Markdown looking for http links. When it finds them, it submits them
-- to webcitation.org / https://secure.wikimedia.org/wikipedia/en/wiki/WebCite
--
-- Limitations:
-- * WebCite requires an email, for notifying the archiver whether the request
-- was successful or not. Currently I hardwire in my own personal email.
-- * Only parses Markdown, not ReST or any other format; this is because 'readMarkdown'
-- is hardwired into it as well.
--
-- By: Gwern Branwen; placed in the public domain

module Archiver (plugin) where

import Gitit.Interface (liftIO, processWithM, Plugin(PreCommitTransform), Inline(Link))
import Control.Monad (when)
import Network.URI (isURI)
import Control.Concurrent (forkIO)
import Network.HTTP (getRequest, simpleHTTP)
import Text.Pandoc (defaultParserState, readMarkdown)
import Control.Monad.Trans (MonadIO)

plugin :: Plugin
plugin = PreCommitTransform archivePage 

archivePage :: (MonadIO m) => String -> m String
archivePage x = do let p = readMarkdown defaultParserState x
                       -- force evaluation and archiving side-effects
                   _p' <- liftIO $ processWithM archiveLinks p
                   return x -- note: this is read-only - don't actually change page!

archiveLinks :: Inline -> IO Inline
archiveLinks x@(Link _ (ln, _)) = checkArchive ln >> return x
archiveLinks x = return x

-- | Error check the URL.
checkArchive :: (MonadIO m) => String -> m ()
checkArchive a = when (isURI a) (liftIO $ archiveURL a)

archiveURL :: String -> IO ()
archiveURL url = forkIO (openURL ("http://www.webcitation.org/archive?url=" ++ url ++ "&email=gwern0@gmail.com") >> return()) >> return ()
   where openURL = simpleHTTP . getRequest