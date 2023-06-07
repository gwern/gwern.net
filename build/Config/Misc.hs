{-# LANGUAGE OverloadedStrings #-}
module Config.Misc where

import qualified Data.Text as T (head, takeWhile, Text)

import Utils (anyInfixT, anyPrefixT, anySuffixT)

-- for Columns.hs:
listLengthMaxN :: Int
listLengthMaxN = 75
-- sublistsLengthMinN :: Int -- TODO: dead config variable?
-- sublistsLengthMinN = 8

-- generateBacklinks.hs:
backlinkBlackList :: T.Text -> Bool
backlinkBlackList "" = error "generateBacklinks.hs (Config.Misc): backlinkBlackList: Called with an empty string! This should never happen."
backlinkBlackList e
  | anyInfixT f ["/backlink/", "/link-bibliography/", "/similar/", "wikipedia.org/wiki/"] = True
  | anyPrefixT f ["$", "#", "!", "mailto:", "irc://", "\8383", "/doc/www/", "/newsletter/", "/changelog", "/mistakes", "/traffic", "/me", "/lorem",
                   -- WARNING: do not filter out 'metadata/annotation' because that leads to empty databases & infinite loops
                   "/static/404", "https://www.dropbox.com/", "https://dl.dropboxusercontent.com/", "/confidential/", "/private/", "/secret/"] = True
  | anySuffixT f ["/index", "/index-long"] = True
  | otherwise = False
  where f = if T.head e == '#' then e else T.takeWhile (/= '#') e -- drop anchors to avoid spurious mismatches eg. '/index#backlink-id-of-some-sort' would bypass a mere '"/index" `isSuffixOf`' check without this.

-- generateDirectory.hs:
-- at what number of links should we auto-collapse the '# Miscellaneous' section because it adds so many entries to the page on load if left uncollapsed?
miscellaneousLinksCollapseLimit :: Int
miscellaneousLinksCollapseLimit = 50

-- generateLinkbibliography.hs:
-- don't waste the user's time if the annotation is not heavily linked, as most are not, or if all the links are WP links:
mininumLinkBibliographyFragment :: Int
mininumLinkBibliographyFragment = 3

userAgent :: String
userAgent = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:110.0) Gecko/20100101 Firefox/110.0"
