{-# LANGUAGE OverloadedStrings #-}

module LinkIcon (addIcon, linkIcon, linkIconTest, linkIconPrioritize) where

import Data.List (sort)
import qualified Data.Map.Strict as M (toList, fromListWith, map)
import Data.Maybe (fromJust)
import qualified Data.Text as T (isInfixOf, isPrefixOf, Text)
import Text.Pandoc (Inline(Link), nullAttr)

import LinkBacklink (readBacklinksDB)
import Utils (host, hasKeyAL, anyPrefixT, inlinesToText, removeKey, addClass)
import qualified Config.LinkIcon as C (prioritizeLinkIconMin, prioritizeLinkIconBlackList, overrideLinkIcons, linkIconTestUnitsText, linkIconRules)

-- Statically, at site 'compile-time', define the link-icons for links. Doing this at runtime with CSS is
-- entirely possible and originally done by links.css, but the logic becomes increasingly convoluted
-- & bug-prone because of CSS properties like cascading & longest-matches, and exceptions like
-- 'organization icon overrides PDF icon' become fertile sources of errors & regressions.
-- Doing this at compile-time in Haskell is easier and also reduces performance burden on the client
-- browser. For a more detailed discussion of the problems & solution, and history of prior link-icon
-- implementations, see <https://gwern.net/design-graveyard#link-icon-css-regexps>.

-- Rules for URL→icon. All supported examples: <https://gwern.net/lorem-link>
-- Supported icon types:
-- - "svg" (+$NAME of the SVG filename in </static/img/icons/$NAME>; must be dark-mode compatible);
-- - "text"+(1-4 Unicode characters) + comma-separated modifiers; text supports additional control:
--   - "sans" (given Gwern.net's default font is Source Serif Pro, it is serif by default, while many logotypes are deliberately sans so this enables Source Sans Pro),
--   - "mono" (IBM Plex),
--   - "italic" (serif italic weight),
--   - "overline",
--   - "tri" (for 3-letters, squeezing in horizontally),
--   - "quad" (turned into a 2×2 grid).
--  Most combinations will be valid so one can write "text,quad,mono" (eg. for a computing organization like 'IEEE'). Text effects beyond this can usually be achieved by some Unicode trickery, such as adding in HAIR SPACEs or using BOLD versions of characters. Emoji should also work with appropriate combining-characters but can be tricky to get working reliably cross-platform.
--
-- Rules: arbitrary pure Haskell can be used to match, and the order of rules matters to allow overrides/precedence (first rule to match wins, so higher=stronger); convenience helpers are provided to match a domain(s), anywhere(s) infix, or by extension(s). These also check for malformedness.
--
-- All rules MUST have a test-case exercising each sub-rule (if multiple domains are matched, each domain should have a test-case). Only one testcase is necessary in /lorem-link (because that's just testing that the link-icon itself looks right rendered by browsers, and not that said link-icon is being put on all the links it should be).
--
-- HTML/CSS implementation details:
-- Based on </static/js/links.js>. Text and SVG are styled as groups in </static/css/links.css>, and individual text-strings or SVGs can be styled individually (as is usually required).
-- The idea is to annotate every `<a>` with two new `data-` attributes, `data-link-icon` and
-- `data-link-icon-type` which jointly specify the type & content of the icon. The link-icon for
-- 'svg' type is overloaded to be a filename in `/static/img/icon/$LINKICON.svg`.
--
-- λ linkIcon $ Link nullAttr [Str "foo"] ("https://forum.evageeks.org/forum?id=2222", "")
-- Link ("",[],[("link-icon","EG"),("link-icon-type","text")]) [Str "foo"] ("https://forum.evageeks.org/forum?id=2222","")
-- λ linkIcon $ Link nullAttr [Str "foo"] ("/doc/foo.pdf", "Foo & Bar 2022")
-- → Link ("",[],[("link-icon","pdf"),("link-icon-type","svg")]) [Str "foo"] ("/doc/foo.pdf","Foo & Bar 2022")
-- → <a href="/doc/foo.pdf" data-link-icon="pdf" data-link-icon-type="svg" title="Foo &amp; Bar 2022">foo</a>
--
-- URL rewrite handling:
-- In cases of local archive links, matches on the `/doc/www/$DOMAIN/$ARCHIVE.html` aren't necessarily *exactly*
-- as powerful; local archives deliberately throw away sub-directory structure for simpler addresses, so 2 matches for
-- 'foo.com/bar/*' and 'foo.com/quux/*' would collide when trying to match just '/doc/www/foo.com/$ARCHIVE.html'.
-- For this case, we detect & exploit the `data-original-URL` attribute which is around for just such problems,
-- and we run matches on the original URL, and everything should work as expected then.
--
-- TODO: the PDF checks are incomplete (and only look for ".pdf" essentially) but it would require IO or perhaps
-- a caching database to actually detect what MIME type a live URL returns, which is a PITA, and since I'm trying
-- to remove all weird non-standard PDFs and host locally all PDFs with clean names & extensions,
-- maybe that's a vestigial concern?
-- TODO: refactor into multiple functions, like 'linkIconOrg', 'linkIconQuad' etc, and then move into Config.LinkIcon:
linkIcon :: Inline -> Inline
linkIcon x@(Link (_,cl,_) _ (u, _))
 -- Short-circuits for manual control (one can either disable icons with a `[Foo](URL){.icon-not}`
 -- class, or specify a preferred icon on a link, like `[Foo](URL){.link-icon="deepmind"
 -- .link-icon-type="svg"}` by specifying the attributes directly), or define a global URL/(link
 -- icon, link icon type) rewrite:
 | "icon-not" `elem` cl = x
 -- NOTE: 'gwern': the Fraktur '𝔊' SVG logo (used to be the Unicode icon but looks a bit fuzzy & squashed as a link-icon so has been replaced by an edit of the Gwern.net logo) for local essay links (where 'local' is defined as '/' but with no '.' in it) is set dynamically client-side by rewrite.js:l1075 (`designateSpecialLinkIcons`) and so we do not handle it here. (It is also overridden by 'icon-not'; WARNING: 'icon-not' is used at runtime and should not be erased!)
 | hasIcon x           = x
 | hasKeyAL u C.overrideLinkIcons = let i = fromJust $ lookup u C.overrideLinkIcons in addIcon x i
 | anyPrefixT u ["/metadata/annotation/"] = x

 | "directory-indexes-upwards"   `elem` cl = addIcon x ("arrow-up-left", "svg")
 | "directory-indexes-downwards" `elem` cl = addIcon x ("arrow-down-right", "svg")
 | "directory-indexes-sideways"  `elem` cl = addIcon x ("arrow-right", "svg")

 | otherwise = removeIconDuplicate $ addIcon x $ C.linkIconRules u
linkIcon x = x

-- Periodically like newspapers are often referenced by their abbreviation, which may also be their text link-icon;
-- it is unnecessary to have a link with a redundant link-icon, eg. 'the WSJ^WSJ^ reported today...' These would
-- be tedious to try to remember to annotate every instance with a `.icon-not`, so we check for that redundancy,
-- and remove or skip the text link-icon.
--
-- This doesn't apply to symbolic link-icons (eg. 'the NYT^icon^ reported today...' is probably not worth bothering trying to hide).
removeIconDuplicate :: Inline -> Inline
removeIconDuplicate x@(Link (_,_,kvs) text _) = let iconType = filter (\(k,v) -> k == "link-icon-type" && "text" `T.isPrefixOf` v) kvs
                                                in if null iconType then x else
                                                     let iconText = head $ map snd $ filter (\(k,_) -> k == "link-icon") kvs
                                                         text' = inlinesToText text
                                                     in if iconText /= text' then x else removeIcon x
removeIconDuplicate x = x

hasIcon :: Inline -> Bool
hasIcon (Link (_,_,ks) _ (_,_)) =
  case lookup "link-icon" ks of
    Just _ -> True
    Nothing -> case lookup "link-icon-type" ks of
                 Just _ -> True
                 Nothing -> False
hasIcon _ = True

hasIconURL :: T.Text -> Bool
hasIconURL = hasIcon . getIcon

getIcon :: T.Text -> Inline
getIcon u = linkIcon $ Link nullAttr [] (u,"")

addIcon :: Inline -> (T.Text, T.Text) -> Inline
addIcon x ("", "") = x
addIcon x@(Link (idt,cl,ks) a (b,c)) (icon, iconType)  =
  if hasIcon x then x else Link (idt,cl,
                                  [("link-icon",icon), ("link-icon-type",iconType)] ++
                                  ks) a (b,c)
addIcon x _ = x

-- remove any existing icon metadata from a Link, and add `.icon-not` to ensure it can't show up again
-- should any later passes try to add icons again.
removeIcon :: Inline -> Inline
removeIcon x@Link{} = addClass "icon-not" $ removeKey "link-icon" $ removeKey "link-icon-type" x
removeIcon x        = x

-- to find URLs worth defining new link icons for, pass through a list of URLs (perhaps extracted
-- from the backlinks database) and return domains with at least `linkIconMin` matches. (Link icons
-- are enough work that below a certain level of prevalence, they are not worthwhile even if completely
-- transparent/self-explanatory.)
--
-- The original raw results are particularly useful when piped into <https://gwern.net/haskell/lcp.hs> to
-- get suggested prefixes/domains worth adding link-icons for, or one can just look at the domains by `host`:
linkIconPrioritize :: IO [(Int,T.Text)]
linkIconPrioritize = do b <- LinkBacklink.readBacklinksDB
                        let b' = M.toList $ M.map length b
                        let b'' = map (\(y,z) -> (host y,z)) $ filter (\(url,_) ->  host url `notElem` C.prioritizeLinkIconBlackList &&
                                                                                    not (hasIconURL url) &&
                                                                                    ("." `T.isInfixOf` url)) b'
                        let b''' =  M.fromListWith (+) b''
                        return $ reverse $ sort $ filter (\(e,f) -> e >= C.prioritizeLinkIconMin && f /="") $ map (\(c,d) -> (d,c)) $ M.toList b'''

-- Test suite:
--
-- Test the /lorem#link-icons test cases as unit-tests of `linkIcon`: it should, for every URL
-- unit-test, generate the specified link-icon/link-icon-type. Return the list of mismatches for
-- fixing.
-- Here we test that URLs get assigned the appropriate icons; on /lorem, we render them to check for
-- CSS/visual glitches. Any new test-cases should be added to both (with different URLs where possible).
--
-- TODO: does not test more complex behavior like suppression of redundant link-icons
linkIconTest :: [(T.Text,T.Text,T.Text)]
linkIconTest = filter (\(url, li, lit) -> linkIcon (Link nullAttr [] (url,""))
                                          /=
                                          Link ("",[], [("link-icon",li), ("link-icon-type", lit)]) [] (url,"")
                                                   )
               C.linkIconTestUnitsText
