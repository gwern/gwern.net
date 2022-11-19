module LinkMetadataTypes where

import qualified Data.Map.Strict as M

type Metadata = M.Map Path MetadataItem                                --
type MetadataItem = (String, String, String, String, [String], String) -- (Title, Author, Date, DOI, Tags, Abstract)
type MetadataList = [(Path, MetadataItem)]
type Path = String
