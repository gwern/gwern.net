module LinkMetadataTypes where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M (Map)
import qualified Data.Text as T (unpack, Text)
import System.FilePath (takeExtension)

import Utils (replace)

type Metadata = M.Map Path MetadataItem                                --
type MetadataItem = (String, String, String, String, [String], String) -- (Title, Author, Date, DOI, Tags, Abstract)
type MetadataList = [(Path, MetadataItem)]
type Path = String

data Failure = Temporary | Permanent deriving Show

isPagePath :: T.Text -> Bool
isPagePath f = let f' = replace "https://gwern.net" "" $ T.unpack f in
    (if
        not ("/" `isPrefixOf` f') ||
      ("/static/" `isPrefixOf` f')
     then False else
       takeExtension f' == "")
