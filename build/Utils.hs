{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Control.Monad (when)
import Data.Text.IO as TIO (readFile, writeFile)
import qualified Data.Text as T (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeDirectory)
import System.IO.Temp (emptySystemTempFile)
import Text.Pandoc (def, nullMeta, runPure,
                    writerColumns, writePlain, Block, Pandoc(Pandoc))
import System.IO (stderr, hPutStrLn)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import System.IO.Unsafe (unsafePerformIO)

-- Auto-update the current year.
currentYear :: Int
currentYear = unsafePerformIO $ fmap ((\(year,_,_) -> fromInteger year) . toGregorian . utctDay) Data.Time.Clock.getCurrentTime

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

simplified :: Block -> T.Text
simplified i = simplifiedDoc (Pandoc nullMeta [i])

simplifiedDoc :: Pandoc -> T.Text
simplifiedDoc p = let md = runPure $ writePlain def{writerColumns=100000} p in -- NOTE: it is important to make columns ultra-wide to avoid formatting-newlines being inserted to break up lines mid-phrase, which would defeat matches in LinkAuto.hs.
                         case md of
                           Left _ -> error $ "Failed to render: " ++ show md
                           Right md' -> md'

-- print normal progress messages to stderr in bold green:
printGreen :: String -> IO ()
printGreen s = hPutStrLn stderr $ "\x1b[32m" ++ s ++ "\x1b[0m"

-- print danger or error messages to stderr in red background:
printRed :: String -> IO ()
printRed s = hPutStrLn stderr $ "\x1b[41m" ++ s ++ "\x1b[0m"

-- Repeatedly apply `f` to an input until the input stops changing.
-- <https://stackoverflow.com/questions/38955348/is-there-a-fixed-point-operator-in-haskell>
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint = until =<< ((==) =<<)
