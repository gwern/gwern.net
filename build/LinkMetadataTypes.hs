module LinkMetadataTypes where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M (Map)
import qualified Data.Text as T (pack, unpack, Text)
import System.FilePath (takeExtension)

import Utils (isLocal, anyInfix, delete)

type ArchiveMetadataItem = Either
  Integer -- Age: first seen date -- ModifiedJulianDay, eg. 2019-11-22 = 58810
  (Maybe FilePath) -- Our archive of the URL: local archive path (if successful, otherwise, should be skipped - already dead?)
type ArchiveMetadataList= [(Path, -- URL: original raw URL of an external link
                            ArchiveMetadataItem)] -- date/path
type ArchiveMetadata = M.Map Path ArchiveMetadataItem

type Metadata = M.Map Path MetadataItem
type MetadataItem = (String, String, String, String, [(String,String)], [String], String) -- (Title, Author, Date, Date-MetadataItem-Created, K-Vs, Tags, Abstract)
type MetadataList = [(Path, MetadataItem)]
type Path = String

-- the size of each URL if known, in bytes and as a percentile; percentiles are normalized within 3 classes:
-- essays (Markdown), local file sizes (PDFs, HTML files etc), and local-archive snapshots of external URLs (taking into account the split-HTML archives as well).
type SizeDB = M.Map FilePath (Int,Int)

data Failure = Temporary | Permanent deriving Show

-- local essays: this excludes locally-hosted files such as PDFs.
-- We define a local essay as a local page, not in /static/, which also has no file extension. All files are guaranteed to have an extension, and extensions are stripped from Markdown pages after compilation ('/foo.md' → '/foo'), so therefore a local filepath with no extension = a Markdown-compiled essay page.
isPagePath :: T.Text -> Bool
isPagePath f = let f' = delete "https://gwern.net" $ T.unpack f in
                 (not (not ("/" `isPrefixOf` f') || ("/static/" `isPrefixOf` f')) &&
                   (takeExtension f' == ""))

-- Does this local file have a perfect HTML substitute usable for in-browser viewing, especially file-transclusion?
-- This is to be distinguished from syntax-highlighted versions of files.
-- If True, then `$FILE.html` must exist and be of high-quality for the reader, and an acceptable replacement for the original.
hasHTMLSubstitute :: FilePath -> Bool
hasHTMLSubstitute f = isLocal (T.pack f) && anyInfix f [".csv", ".doc", ".docx", ".ods", ".xls", ".xlsx"] -- converted by LibreOffice to HTML, see `/static/build/sync.sh`
