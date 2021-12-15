{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Control.Monad (when)
import Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Text as T (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory)
import System.IO.Temp (emptySystemTempFile)

-- Write only when changed, to reduce sync overhead; creates parent directories as necesary; writes to tempfile in /tmp/ (at a specified template name), and does an atomic rename to the final file.
writeUpdatedFile :: String -> FilePath -> T.Text -> IO ()
writeUpdatedFile template target contentsNew =
  do existsOld <- doesFileExist target
     if not existsOld then do
       createDirectoryIfMissing True (takeDirectory target)
       TIO.writeFile target contentsNew
       else do contentsOld <- TIO.readFile target
               when (contentsNew /= contentsOld) $ do tempPath <- emptySystemTempFile ("hakyll-"++template)
                                                      TIO.writeFile tempPath contentsNew
                                                      renameFile tempPath target
