module LinkMetadataTypes where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M (Map)
import qualified Data.Text as T (unpack, Text)
import System.FilePath (takeExtension)

import Utils (replace)

type ArchiveMetadataItem = Either
  Integer -- Age: first seen date -- ModifiedJulianDay, eg. 2019-11-22 = 58810
  (Maybe FilePath) -- Our archive of the URL: local archive path (if successful, otherwise, should be skipped - already dead?)
type ArchiveMetadataList= [(Path, -- URL: original raw URL of an external link
                            ArchiveMetadataItem)] -- date/path
type ArchiveMetadata = M.Map Path ArchiveMetadataItem

type Metadata = M.Map Path MetadataItem                                --
type MetadataItem = (String, String, String, String, [String], String) -- (Title, Author, Date, DOI, Tags, Abstract)
type MetadataList = [(Path, MetadataItem)]
type Path = String

data Failure = Temporary | Permanent deriving Show

isPagePath :: T.Text -> Bool
isPagePath f = let f' = replace "https://gwern.net" "" $ T.unpack f in
                 (not (not ("/" `isPrefixOf` f') || ("/static/" `isPrefixOf` f')) &&
                   (takeExtension f' == ""))
