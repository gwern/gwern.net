-- | Scans page of Markdown looking for http links. When it finds them, it submits them
-- to webcitation.org / https://secure.wikimedia.org/wikipedia/en/wiki/WebCite
--
-- Limitations: 
-- * WebCite requires an email, for notifying the archiver whether the request
-- was successful or not. Currently I hardwire in my own personal email.
-- * Only parses Markdown, not ReST or any other format; this is because 'readMarkdown'
-- is hardwired into it as well.
module Archiver (plugin) where

import Gitit.Interface
import Control.Monad
import Network.URI (isURI)
import Control.Concurrent
import Network.HTTP -- .simpleHTTP
import Text.Pandoc
import Text.Pandoc.Definition
import Control.Monad.Trans

plugin :: Plugin
plugin = PreCommitTransform (\a ->archivepage a >> return a)

archivepage :: (MonadIO m) => String -> m String
archivepage x = do let p = readMarkdown defaultParserState x 
                       -- force evaluation and archiving side-effects
                   _p' <- liftIO $ processWithM archivelinks p
                   return x

archivelinks :: Inline -> IO Inline
archivelinks x@(Link _ (ln, _)) = archive ln >> return x
archivelinks x = return x

-- error check
archive :: (MonadIO m) => String -> m ()
archive [] = return ()
archive a = when (isURI a) (liftIO $ archiveURL a)

archiveURL :: String -> IO ()
archiveURL url = forkIO (openURL("http://www.webcitation.org/archive?url=" ++ url ++ "&email=gwern0@gmail.com"  ) >> return()) >> return ()
   where openURL = Network.HTTP.simpleHTTP . getRequest

