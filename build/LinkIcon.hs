{-# LANGUAGE OverloadedStrings #-}

module LinkIcon (addIcon, linkIcon, linkIconTest, linkIconPrioritize) where

import Data.Char (isDigit, isHexDigit)
import Data.List (sort)
import qualified Data.Map.Strict as M (toList, fromListWith, map)
import Data.Maybe (fromJust)
import qualified Data.Text as T (isInfixOf, isPrefixOf, Text, splitOn, unpack, null, toLower, head, length)
import Text.Pandoc (Inline(Link), nullAttr)

import LinkBacklink (readBacklinksDB)
import Utils (host, hasKeyAL, anyPrefixT, inlinesToText, removeKey, addClass, isURLAny)
import qualified Config.LinkIcon as C (prioritizeLinkIconMin, prioritizeLinkIconBlackList, overrideLinkIcons, linkIconTestUnitsText, linkIconRules, linkIconTypes)

-- Statically, at site 'compile-time', define the link-icons for links. Doing this at runtime with CSS is
-- entirely possible and originally done by links.css, but the logic becomes increasingly convoluted
-- & bug-prone because of CSS properties like cascading & longest-matches, and exceptions like
-- 'organization icon overrides PDF icon' become fertile sources of errors & regressions.
-- links.css was replaced by links.js as an intermediate step, to document all of the rules & interactions.
-- links.js was then replaced by LinkIcon at compile-time.
-- Doing this at compile-time in Haskell is easier and also reduces performance burden on the client
-- browser. For a more detailed discussion of the problems & solution, and history of prior link-icon
-- implementations, see <https://gwern.net/design-graveyard#link-icon-css-regexps>.
--
-- We attempt to use as simple monochrome icons as possible to fit into the Gwern.net overall theme and reduce clutter & maintenance. We prefer typographical ones which can be inlined, will look familiar, and don't require a custom SVG (which would bloat the SVG sprite sheet). We prioritize domains by their frequency, and regularly review them; if a domain can't be given a good link icon, we add it to the review blacklist to ignore them. We try to avoid being overly clever: if it is not obvious to us what a link-icon should be, then that means having one is wrong! (Unless we want to make an exception, like because it is for site infrastructure, the reader may learn it quickly, or we just like it too much to not have a link-icon.)

-- Rules for URL → icon. All supported examples: <https://gwern.net/lorem-link>
-- Supported icon types:
-- - "svg" (+$NAME of the SVG filename in </static/img/icons/$NAME>; must be dark-mode compatible);
-- - "text"+(1–4 Unicode characters) + comma-separated modifiers; text supports additional control:
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
-- All rules MUST have a test-case exercising each sub-rule (if multiple domains are matched, each domain should have a test-case). Only one test-case is necessary in /lorem-link (because that's just testing that the link-icon itself looks right rendered by browsers, and not that said link-icon is being put on all the links it should be).
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
-- For this case, we detect & exploit the `data-url-original` attribute which is around for just such problems,
-- and we run matches on the original URL, and everything should work as expected then.
linkIcon :: Inline -> Inline
linkIcon x@(Link (_,cl,attributes) _ (u, _))
 -- Short-circuits for manual control (one can either disable icons with a `[Foo](URL){.icon-not}`
 -- class, or specify a preferred icon on a link, like `[Foo](URL){.link-icon="deepmind"
 -- .link-icon-type="svg"}` by specifying the attributes directly), or define a global URL/(link
 -- icon, link icon type) rewrite:
 | "icon-not" `elem` cl = x
 -- NOTE: 'gwern': the Fraktur '𝔊' SVG logo (used to be the Unicode icon but looks a bit fuzzy & squashed as a link-icon so has been replaced by an edit of the Gwern.net logo) for local essay links (where 'local' is defined as '/' but with no '.' in it) is set dynamically client-side by rewrite.js:l1075 (`designateSpecialLinkIcons`) and so we do not handle it here. (It is also overridden by 'icon-not'; WARNING: 'icon-not' is used at runtime and should not be erased!)
 | hasIcon x           = x
 | hasKeyAL u C.overrideLinkIcons = let i = fromJust $ lookup u C.overrideLinkIcons in addIcon x i
 -- lb/bl/sl:
 | u == "#link-bibliography" || u == "/design#link-bibliographies" || "/metadata/annotation/link-bibliography/" `T.isPrefixOf` u = addIcon x ("bibliography", "svg", "") -- original SVG: "List" <https://thenounproject.com/icon/list-4184262/>, Paisley (CC-BY)
 | u == "#similars" || u == "/design#similar-links" || "/metadata/annotation/similar/" `T.isPrefixOf` u = addIcon x ("≈", "text", "") -- ALMOST EQUAL TO: recommendations/similar-links which are 'similar' or 'almost equal to' the current URL; NOTE: hardcoded in `default.html` because the link-icon pass may not run there
 | u == "#backlinks" || u == "/design#backlink" || "/metadata/annotation/backlink/" `T.isPrefixOf` u = addIcon x ("arrows-pointing-inwards-to-dot", "svg", "") -- an 'implosion' arrow icon to indicate multiple links 'in' to the current article (as opposed to the normal forwardlinks 'out')
 | anyPrefixT u ["/metadata/annotation/"] = x
 | "directory-indexes-upwards"   `elem` cl = addIcon x ("arrow-up-left", "svg", "")
 | "directory-indexes-downwards" `elem` cl = addIcon x ("arrow-down-right", "svg", "")
 | "directory-indexes-sideways"  `elem` cl = addIcon x ("arrow-right", "svg", "")
 | T.head u == '#' = x
 | otherwise = if "http" `T.isPrefixOf` originalURL && not (isURLAny $ T.unpack originalURL)
               then error $ "LinkIcon.linkIcon: input was not a valid URL? " ++ show x
               else removeIconDuplicate $ addIcon x $ C.linkIconRules originalURL
 where originalURL :: T.Text -- NOTE: all rules are defined in terms of the original canonical URL, without local archives in mind. So to cooperate with LinkArchive, we must swap the target if LA swapped it first:
       originalURL = case lookup "data-url-original" attributes of
                       Nothing   -> u
                       Just url -> url
linkIcon x = x

-- Periodically like newspapers are often referenced by their abbreviation, which may also be their text link-icon;
-- it is unnecessary to have a link with a redundant link-icon, eg. 'the WSJ^WSJ^ reported today...' These would
-- be tedious to try to remember to annotate every instance with a `.icon-not`, so we check for that redundancy,
-- and remove or skip the text link-icon.
--
-- This doesn't apply to symbolic link-icons (eg. 'the NYT^icon^ reported today...' is probably not worth bothering trying to hide). So even if the SVG filename is identical to the anchor text, we use the SVG anyway (because it is presumably stylized or different somehow from the literal text).
removeIconDuplicate :: Inline -> Inline
removeIconDuplicate x@(Link (_,_,kvs) text _) = let iconType = filter (\(k,v) -> k == "link-icon-type" && "text" `T.isPrefixOf` v) kvs
                                                in if null iconType then x else
                                                     let iconText = head $ map snd $ filter (\(k,_) -> k == "link-icon") kvs
                                                         text' = inlinesToText text
                                                     in if iconText /= text' && (snd (head iconType) /= "svg") then x else removeIcon x
removeIconDuplicate x = x

-- whether a Link has a link-icon set already; errors out if the attribute keys are set but have empty values (to help guard against that possible error). We require either a link-icon + link-icon-type (+ optional link-icon-color), *or* a link-icon color (for cases where there is a reasonable on-hover color but not icon)
hasIcon :: Inline -> Bool
hasIcon (Link (_,_,ks) _ (_,_)) =
  case lookup "link-icon" ks of
    Just "" -> case lookup "link-icon-color" ks of
                 Just "" -> error "LinkIcon.hasIcon: Empty `link-icon` *and* `link-icon-color` attribute; this should never happen!"
                 Just _  -> True
                 Nothing -> error "LinkIcon.hasIcon: Empty `link-icon` *but* no `link-icon-color` attribute; this should never happen!"
    Just _  -> True
    Nothing -> case lookup "link-icon-type" ks of
                 Just "" -> error "LinkIcon.hasIcon: Empty `link-icon-type` attribute; this should never happen!"
                 Just _  -> True
                 Nothing -> case lookup "link-icon-color" ks of
                   Nothing -> False
                   Just "" -> error "LinkIcon.hasIcon: Empty `link-icon-color` string; this should never happen!"
                   Just _ -> True
hasIcon _ = True

hasIconURL :: T.Text -> Bool
hasIconURL = hasIcon . getIcon

getIcon :: T.Text -> Inline
getIcon u = linkIcon $ Link nullAttr [] (u,"")

addIcon :: Inline -> (T.Text, T.Text, T.Text) -> Inline
addIcon x ("", "", "") = x
addIcon x@(Link (idt,cl,ks) a (b,c)) (icon, iconType, iconColor)  =
  if hasIcon x then x else Link (idt,cl,
                                  ((if T.null icon       then [] else [("link-icon",      icon)]) ++
                                    (if T.null iconType  then [] else [("link-icon-type", iconType)]) ++
                                    (if T.null iconColor then [] else [("link-icon-color",T.toLower iconColor)])) ++ -- enforce lowercase (rather than mixed or uppercase) convention of RGB hex notation
                                  ks) a (b,c)
addIcon x _ = x

-- remove any existing icon metadata from a Link, and add `.icon-not` to ensure it can't show up again
-- should any later passes try to add icons again.
removeIcon :: Inline -> Inline
removeIcon x@Link{} = addClass "icon-not" $ removeKey "link-icon-color" $ removeKey "link-icon" $ removeKey "link-icon-type" x
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
-- We also check that all of the target styling results like `"text,quad,mono"` parse to only valid
-- entries of the enumeration `C.linkIconTypes`, to avoid issues like accidentally writing `"text,quad,monospace"`
-- which would not trigger the unit-test and looks correct (because stringly-typed), but is wrong.
--
-- TODO: does not test more complex behavior like suppression of redundant link-icons
linkIconTest :: [(T.Text,T.Text,T.Text,T.Text)]
linkIconTest = filter (\(url, li, lit, litc) ->
                         if not (all (`elem` C.linkIconTypes) (T.splitOn "," lit)) then error ("LinkIcon.linkIconTest: the type string contains unknown kinds of formatting commands. Original: " ++ show lit)
                         else
                         (linkIcon . linkIcon . linkIcon . -- idempotent test: that it is correct even after multiple passes
                           linkIcon) (Link nullAttr [] (url,""))
                                          /=
                                          Link ("",[], ((if T.null li then [] else [("link-icon",li)]) ++
                                                        (if T.null lit then [] else [("link-icon-type", isValidIconType li lit)]) ++
                                                        (if T.null litc then [] else [("link-icon-color",isValidCssHexColor litc)])) -- note: we only call this in `linkIconTest` to avoid runtime overhead.
                                               ) [] (url,"")
                      )
               C.linkIconTestUnitsText

isValidIconType :: T.Text -> T.Text -> T.Text
isValidIconType _ "" = error "LinkIcon.isValidIconType: passed an empty string for link-icon-type. This should never happen!"
isValidIconType "" _ = error "LinkIcon.isValidIconType: passed an empty string for link-icon. This should never happen!"
isValidIconType li lit
 | "quad" `elem` types = if T.length li == 4 then lit else error $ "LinkIcon.isValidIconType: quad icon's link-text was not exactly 4 characters. Inputs: " ++ show li ++ "; " ++ show lit
 | "tri" `elem` types  = if T.length li == 3 then lit else error $ "LinkIcon.isValidIconType: tri icon's link-text was not exactly 3 characters. Inputs: " ++ show li ++ "; " ++ show lit
 -- if it's not a text quad/tri, nor an SVG, then it must be a text icon of exactly 1 or 2 letters:
 | "text" `elem` types = if T.length li == 1 || T.length li == 2 then lit else error $ "LinkIcon.isValidIconType: non-SVG non-tri/quad icon's link-text was not exactly 1 or 2 characters. Inputs: " ++ show li ++ "; " ++ show lit
 | otherwise           = lit
 where types = T.splitOn "," lit

-- check that a string is a valid CSS color in either '#RGB' or '#RRGGBB' format.
-- (Note that we allow uppercase 'A–F' but we emit only lowercase 'a–f' for consistency/readability.)
isValidCssHexColor :: T.Text -> T.Text
isValidCssHexColor ""    = ""
isValidCssHexColor color = case T.unpack color of
    '#':rest -> if length rest /= 3 && length rest /= 6 then error $ "LinkIcon.isValidCssHexColor: hex value was an invalid length, neither 3 nor 6? Original input was: " ++ show color
                else if not (all isHexDigit rest) then
                       if length rest == 8 then
                         error $ "LinkIcon.isValidCssHexColor: hex value was length-8; does this have 2 opacity values at the end, and is RGBA? Convert to RGB if so. Original input was: " ++ show color
                       else
                         error $ "LinkIcon.isValidCssHexColor: hex value was proper length, but contained non-hexadecimal characters? Original input was: " ++ show color
                     else if color `elem` ["#ffffff", "#000000"] then error $ "LinkIcon.isValidCssHexColor: failed blacklist check of colors which are usually invalid as link-icon colors. Double-check this! Input was: " ++ show color
                             else if not (isDistinctColor $ T.unpack color) then error $ "too gray: " ++ show color
                                    else color
    _  -> error $ "LinkIcon.isValidCssHexColor: input CSS hex color failed hex check; did not start with a hash? Original input was: " ++ show color

-- try to detect 'gray'/black/white colors, which have no distinct color as link-icon colors.
-- To implement this, we try a threshold on the channel differences. If all three channels are very close to each other, it’s effectively a grayish color. For example, define a tolerance and check if the difference between the max and min channel values is small. If it’s below a certain threshold, consider it gray. This considers a color “distinct” only if the range between its darkest and brightest channel is greater than 30. You can adjust the threshold to taste.
-- eg. a maximum gray should be something like '#7a7a99' and a minimum is '#00001f'.
isDistinctColor :: String -> Bool
isDistinctColor ['#', r1, r2, g1, g2, b1, b2] =
  let hexVal c = if isDigit c
                 then fromEnum c - fromEnum '0'
                 else 10 + (fromEnum c - fromEnum 'a')
      r = hexVal r1 * 16 + hexVal r2
      g = hexVal g1 * 16 + hexVal g2
      b = hexVal b1 * 16 + hexVal b2
      mx = max r (max g b)
      mn = min r (min g b)
      diff = mx - mn
      threshold = 30
  in diff > threshold
isDistinctColor _ = False
