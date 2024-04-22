{-# LANGUAGE OverloadedStrings #-}
module Config.LinkArchive where

import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, find, delete)
import Utils (sed, anyInfix, anyPrefix, anySuffix, replace)
import Network.URI (parseURI, uriAuthority, uriFragment, uriPath, uriQuery, uriRegName, uriToString, URI, URIAuth)
import Network.HTTP.Types.URI (parseQuery, renderQuery)
import qualified Data.ByteString.Char8 as C8 (pack, unpack)
import qualified Data.Text as T (Text)
import qualified Data.Map.Strict as M (fromList)

import LinkMetadataTypes (ArchiveMetadata, hasHTMLSubstitute)

archiveDelay :: Integer
archiveDelay = 60

-- some URLs are so cheap & easy & reliable to archive that we don't need to count them
-- against our manual-review limit, because we won't meaningfully manually review them.
isCheapArchive :: String -> Bool
isCheapArchive url = f url || f (transformURLsForArchiving url)
  where f u = anyInfix u [".pdf", "#pdf", "freedium.cfd", "news.ycombinator.com", "localhost:8081", "twitter.com",
                                   "https://web.archive.org/web/"] -- see <https://gwern.net/archiving#why-not-internet-archive>

-- sometimes we may want to do automated transformations of a URL *before* we check any whitelists. In the case of
-- Arxiv, we want to generate the PDF equivalent of the HTML abstract landing page, so the PDF gets archived, but then
-- we also want to rewrite it to use the Ar5iv (HTML5 version) service, and provide *both*.
--
-- In the case of OpenReview, the `forum?id=` is the peer reviews, which are worth reading, but we'd like to provide the
-- PDF link too. We don't need a third version, just to provide the two, so this is easier than the Ar5iv rewrite.
-- (Hypothetically, we could do Reddit.com → Old.Reddit.com, or LW → GW rewrites this way too.)
transformURLsForArchiving, -- data-url-archive
  transformURLsForMobile, -- data-href-mobile
  transformURLsForLinking :: String -> String -- data-url-html
transformURLsForArchiving = sed "https://arxiv.org/abs/([0-9]+\\.[0-9]+)(#.*)?" "https://arxiv.org/pdf/\\1.pdf\\2" . sed "https://arxiv.org/abs/([a-z-]+)/([0-9]+).*(#.*)?" "https://arxiv.org/pdf/\\1/\\2.pdf\\3"
                            . replace "https://openreview.net/forum" "https://openreview.net/pdf"
                            -- Old Reddit is the preferred browsing & archiving frontend given the death of `i.reddit.com` & `.compact`
                            . replace "https://www.reddit.com" "https://old.reddit.com"
                            . replace "https://twitter.com/" "http://localhost:8081/"
                            . replace "https://medium.com" "https://freedium.cfd" -- clean Medium frontend; NOTE: we use <freedium.cfd> instead of <scribe.rip> because Scribe (by design) does not cache or save content, only proxies to Medium, so once a Medium article is unpredictably deleted/paywalled, a working Scribe mirror immediately breaks. Freedium will keep working.
                            . sed "^https://(.*)\\.fandom.com/(.*)$" "https://antifandom.com/\\1/\\2" -- clean Wikia/Fandom frontend
                            . sed "^(https://web\\.archive\\.org/web/[12][0-9]+)/http(.*)$" "\\1if_/http\\2" -- <https://en.wikipedia.org/wiki/Help:Using_the_Wayback_Machine#Removing_the_navigational_toolbar>
                            . transformURItoGW

-- `data-href-mobile`:
transformURLsForMobile    = sed "https://arxiv.org/abs/([0-9]+\\.[0-9]+)(#.*)?" "https://browse.arxiv.org/html/\\1?fallback=original\\2" .
  sed "https://arxiv.org/abs/([a-z-]+)/([0-9]+).*(#.*)?" "https://browse.arxiv.org/html/\\1/\\2?fallback=original\\3" . -- handle oddities like hep-ph
  replace "https://twitter.com" "https://nitter.net"

-- `data-url-html`:
transformURLsForLinking   = replace "https://www.reddit.com" "https://old.reddit.com" . -- Old Reddit is much politer to send people to
  -- make IA book/item pages pop up nicer in live-links, by enabling JS, so theater-mode works.
  (\u -> if u `anyPrefix` ["https://archive.org/details/"]    && '#' `notElem` u && not (u `anyInfix` ["?view=theater"]) then u ++ "?view=theater" else u)
  . replace "https://medium.com" "https://freedium.cfd"
  . addAmazonAffiliate
  . sed "^https://(.*)\\.fandom.com/(.*)$" "https://antifandom.com/\\1/\\2" -- clean Wikia/Fandom frontend
  . transformURItoGW
  . transformWPtoMobileWP
  . sed "^(https://web\\.archive\\.org/web/[12][0-9]+)/http(.*)$" "\\1if_/http\\2"
  -- have HTML substitutes (not syntax-highlighted versions); see LinkMetadata.isDocumentViewable filetypes
  . (\u -> if not $ hasHTMLSubstitute u then u else u ++ ".html")

-- called by `LinkArchive.testLinkRewrites`:
localizeLinktestCases :: [(T.Text, -- original URL
                           (T.Text, -- on-disk archive file (if any); absolute for inserting into links, not relative
                            T.Text, -- mobile version
                            T.Text, -- HTML version
                            [T.Text]))] -- classes (just in case, so far)
localizeLinktestCases = [
    ("https://arxiv.org/abs/1909.05858#salesforce",
      ("/doc/www/arxiv.org/0b9e7be08a4baf0b4fc120364ea36172ecb3c9f0.pdf#salesforce", "https://browse.arxiv.org/html/1909.05858?fallback=original#salesforce", "", []))
    , ("https://arxiv.org/abs/1904.01201#facebook", ("/doc/www/arxiv.org/3280474172991f9f5e492000192466bf1d9b6f7d.pdf#facebook", "https://browse.arxiv.org/html/1904.01201?fallback=original#facebook", "", []))
    , ("https://arxiv.org/abs/hep-ph/0204295", ("/doc/www/arxiv.org/4a7da1a80a185d239f989fa3c4773db572c441b0.pdf", "https://browse.arxiv.org/html/hep-ph/0204295?fallback=original", "", []))
    , ("https://scholar.sun.ac.za/server/api/core/bitstreams/6dfdb0ca-e7e5-403e-9a2b-4161e3d93385/content#pdf", ("/doc/www/scholar.sun.ac.za/597ea379e3550e15a6355df58db5b19464dddd42.pdf", "", "", []))
    , ("https://scholar.sun.ac.za/server/api/core/bitstreams/6dfdb0ca-e7e5-403e-9a2b-4161e3d93385/content#pdf", ("/doc/www/scholar.sun.ac.za/597ea379e3550e15a6355df58db5b19464dddd42.pdf", "", "", []))
    , ("https://twitter.com/alexeyguzey/status/1068583101633359874", ("", "https://nitter.net/alexeyguzey/status/1068583101633359874", "", []))
    , ("https://twitter.com/gdb/status/1495821544370708486", ("/doc/www/localhost/26c5938a85b27e976fdbaecb8570d9830362501e.html", "https://nitter.net/gdb/status/1495821544370708486", "", []))
    , ("https://medium.com/@alex.tabarrok/when-can-token-curated-registries-actually-work-%C2%B9-2ad908653aaf", ("/doc/www/freedium.cfd/067a8f86abbb2ba5c0de0ed2f0ccfe046973bfb3.html", "", "https://freedium.cfd/@alex.tabarrok/when-can-token-curated-registries-actually-work-%C2%B9-2ad908653aaf", []))
    , ("https://news.ycombinator.com/item?id=17110385", ("/doc/www/news.ycombinator.com/de1d1ce15816a607ef9cfb9e04c34051ee08211f.html", "", "", []))
    , ("https://openreview.net/forum?id=0ZbPmmB61g#google", ("/doc/www/openreview.net/ec11c5bdd2766cd352fe7df9ae60e748f06d5175.pdf#google", "", "", []))
    , ("https://www.reddit.com/r/AnarchyChess/comments/10ydnbb/i_placed_stockfish_white_against_chatgpt_black/", ("/doc/www/old.reddit.com/bd98124b170baeb9324c51c734083302aa65323a.html", "", "https://old.reddit.com/r/AnarchyChess/comments/10ydnbb/i_placed_stockfish_white_against_chatgpt_black/", []))
    , ("https://darkrunescape.fandom.com/wiki/Doubling_money_scam", ("", "", "https://antifandom.com/darkrunescape/wiki/Doubling_money_scam", []))
    , ("https://archive.org/details/in.ernet.dli.2015.90433", ("", "", "https://archive.org/details/in.ernet.dli.2015.90433?view=theater", []))
    , ("https://www.amazon.com/Exploring-World-Dreaming-Stephen-LaBerge/dp/034537410X/", ("", "", "https://www.amazon.com/Exploring-World-Dreaming-Stephen-LaBerge/dp/034537410X/?tag=gwernnet-20", []))
    , ("https://en.wikipedia.org/wiki/George_Washington", ("", "", "https://en.m.wikipedia.org/wiki/George_Washington#bodyContent", []))
    , ("https://web.archive.org/web/20200928174939/http://thismarketingblogdoesnotexist.com/", ("", "", "https://web.archive.org/web/20200928174939if_/http://thismarketingblogdoesnotexist.com/", []))
    , ("https://web.archive.org/web/20230718144747/https://frc.ri.cmu.edu/~hpm/project.archive/robot.papers/2004/Predictions.html",
       ("/doc/www/web.archive.org/6c2b9128766dab38ecadd896845cfe53920c3ea3.html", "", "https://web.archive.org/web/20230718144747if_/https://frc.ri.cmu.edu/~hpm/project.archive/robot.papers/2004/Predictions.html", []))
    -- LW/GW/EAF:
    , ("https://www.lesswrong.com/posts/WDcXoMdFxkSXPSrwR/n-back-news-jaeggi-2011-or-is-there-a-psychologist?commentId=kuKaKje3en6bnhgFD", ("", "", "https://www.greaterwrong.com/posts/WDcXoMdFxkSXPSrwR/n-back-news-jaeggi-2011-or-is-there-a-psychologist/comment/kuKaKje3en6bnhgFD?format=preview&theme=classic", []))
    , ("https://www.lesswrong.com/posts/mf5LS5pxAy6WxCFNW/what-would-you-do-if-blood-glucose-theory-of-willpower-was", ("", "", "https://www.greaterwrong.com/posts/mf5LS5pxAy6WxCFNW/what-would-you-do-if-blood-glucose-theory-of-willpower-was?format=preview&theme=classic", []))
    , ("https://www.alignmentforum.org/posts/PTkd8nazvH9HQpwP8/building-brain-inspired-agi-is-infinitely-easier-than", ("", "", "https://www.greaterwrong.com/posts/PTkd8nazvH9HQpwP8/building-brain-inspired-agi-is-infinitely-easier-than?format=preview&theme=classic", []))
    , ("https://forum.effectivealtruism.org/posts/dCjz5mgQdiv57wWGz/ingredients-for-creating-disruptive-research-teams", ("", "", "https://ea.greaterwrong.com/posts/dCjz5mgQdiv57wWGz/ingredients-for-creating-disruptive-research-teams?format=preview&theme=classic", []))
    , ("https://arbital.com/p/edge_instantiation/", ("/doc/www/arbital.com/f3415bb9b168d3fcb051b458a48994ec1e8c4611.html", "", "https://arbital.greaterwrong.com/p/edge_instantiation/?format=preview&theme=classic", []))
    , ("https://www.lesswrong.com/posts/thePw6qdyabD8XR4y/interpreting-openai-s-whisper#3_1__Whisper_learns_language_modelling_bigrams", ("", "", "https://www.greaterwrong.com/posts/thePw6qdyabD8XR4y/interpreting-openai-s-whisper?format=preview&theme=classic#3_1__Whisper_learns_language_modelling_bigrams", []))
    ]

localizeLinkTestDB :: ArchiveMetadata
localizeLinkTestDB = M.fromList $
  -- links should not have archives (must be specified to be permanent failures to avoid the link-archive test suite trying to archive them):
  map (\a -> (a,Right Nothing)) ["https://twitter.com/alexeyguzey/status/1068583101633359874", "https://darkrunescape.fandom.com/wiki/Doubling_money_scam", "https://archive.org/details/in.ernet.dli.2015.90433", "https://www.amazon.com/Exploring-World-Dreaming-Stephen-LaBerge/dp/034537410X/", "https://www.lesswrong.com/posts/WDcXoMdFxkSXPSrwR/n-back-news-jaeggi-2011-or-is-there-a-psychologist?commentId=kuKaKje3en6bnhgFD", "https://www.lesswrong.com/posts/mf5LS5pxAy6WxCFNW/what-would-you-do-if-blood-glucose-theory-of-willpower-was", "https://www.alignmentforum.org/posts/PTkd8nazvH9HQpwP8/building-brain-inspired-agi-is-infinitely-easier-than", "https://forum.effectivealtruism.org/posts/dCjz5mgQdiv57wWGz/ingredients-for-creating-disruptive-research-teams", "https://en.wikipedia.org/wiki/George_Washington"]
  -- links which should have archives:
  ++ map (\(a,b) -> (a,Right (Just b))) [("https://arxiv.org/abs/1909.05858#salesforce", "doc/www/arxiv.org/0b9e7be08a4baf0b4fc120364ea36172ecb3c9f0.pdf#salesforce")
                    , ("https://arxiv.org/abs/hep-ph/0204295", "doc/www/arxiv.org/4a7da1a80a185d239f989fa3c4773db572c441b0.pdf")
                    , ("https://scholar.sun.ac.za/server/api/core/bitstreams/6dfdb0ca-e7e5-403e-9a2b-4161e3d93385/content#pdf", "doc/www/scholar.sun.ac.za/597ea379e3550e15a6355df58db5b19464dddd42.pdf")
                    , ("https://medium.com/@alex.tabarrok/when-can-token-curated-registries-actually-work-%C2%B9-2ad908653aaf", "doc/www/freedium.cfd/067a8f86abbb2ba5c0de0ed2f0ccfe046973bfb3.html")
                    , ("https://news.ycombinator.com/item?id=17110385", "doc/www/news.ycombinator.com/de1d1ce15816a607ef9cfb9e04c34051ee08211f.html")
                    , ("https://arxiv.org/abs/1904.01201#facebook", "doc/www/arxiv.org/3280474172991f9f5e492000192466bf1d9b6f7d.pdf#facebook")
                    , ("https://openreview.net/forum?id=0ZbPmmB61g#google", "doc/www/openreview.net/ec11c5bdd2766cd352fe7df9ae60e748f06d5175.pdf#google")
                    , ("https://www.reddit.com/r/AnarchyChess/comments/10ydnbb/i_placed_stockfish_white_against_chatgpt_black/", "doc/www/old.reddit.com/bd98124b170baeb9324c51c734083302aa65323a.html")
                    , ("https://arbital.com/p/edge_instantiation/", "doc/www/arbital.com/f3415bb9b168d3fcb051b458a48994ec1e8c4611.html")
                    , ("https://twitter.com/gdb/status/1495821544370708486", "doc/www/localhost/26c5938a85b27e976fdbaecb8570d9830362501e.html")
                    , ("https://web.archive.org/web/20230718144747/https://frc.ri.cmu.edu/~hpm/project.archive/robot.papers/2004/Predictions.html", "doc/www/web.archive.org/6c2b9128766dab38ecadd896845cfe53920c3ea3.html")
                    ]

-- GreaterWrong provides several mirrors we want to rewrite URLs to: LessWrong, Alignment Forum, Effective Altruism, & Arbital (historical).
-- This function handles them all, rewriting the domain name, and for links to comments, the `?commentId=n` to `/commentId/n` path. For a compact version, we append `?format=preview&theme=classic` or (if there is already a query parameter) `&format=preview&theme=classic`.
--
-- > transformURItoGW "https://www.lesswrong.com/posts/FkgsxrGf3QxhfLWHG/risks-from-learned-optimization-introduction"
-- → "https://www.greaterwrong​.com/posts/FkgsxrGf3QxhfLWHG/risks-from-learned-optimization-introduction?format=preview&theme=classic"
-- transformURItoGW "https://www.lesswrong.com/posts/FkgsxrGf3QxhfLWHG/risks-from-learned-optimization-introduction?foo=bar"
-- → "https://www.greaterwrong.​com/posts/FkgsxrGf3QxhfLWHG/risks-from-learned-optimization-introduction?foo=bar&format=preview&theme=classic"
-- > transformURItoGW "https://forum.effectivealtruism.org/posts/aFYduhr9pztFCWFpz/preliminary-analysis-of-intervention-to-reduce-lead-exposure?commentId=RLdntemEyqFLcCeb9"
-- → "https://ea.greaterwrong​.com/posts/aFYduhr9pztFCWFpz/preliminary-analysis-of-intervention-to-reduce-lead-exposure/comment/RLdntemEyqFLcCeb9?format=preview&theme=classic"
-- > Config.LinkArchive.transformURItoGW "https://arbital.com/p/edge_instantiation/"
-- → "https://arbital.greaterwrong.com/p/edge_instantiation/?format=preview&theme=classic"
-- > transformURItoGW "https://www.lesswrong.com/posts/thePw6qdyabD8XR4y/interpreting-openai-s-whisper#3_1__Whisper_learns_language_modelling_bigrams"
-- → "https://www.greaterwrong.com/posts/thePw6qdyabD8XR4y/interpreting-openai-s-whisper?format=preview&theme=classic#3_1__Whisper_learns_language_modelling_bigrams"
transformURItoGW :: String -> String
transformURItoGW uri = fromMaybe uri $ do
    parsedURI <- parseURI uri
    auth <- uriAuthority parsedURI
    let hostname = uriRegName auth
    mirrorPrefix <- findMirrorPrefix hostname
    let transformedURI = transformToMirrorURI mirrorPrefix parsedURI auth
    let transformedCommentURI = handleCommentId transformedURI
    return $ if shouldTransform hostname uri
    then let fragment = uriFragment transformedCommentURI
             baseURI = transformedCommentURI { uriFragment = "" }
             transformedStr = uriToString id baseURI ""
             queryParams = if null (uriQuery transformedCommentURI)
                           then "?format=preview&theme=classic"
                           else "&format=preview&theme=classic"
         in transformedStr ++ queryParams ++ fragment
    else uri
  where
    originalDomains :: [String]
    originalDomains = [".lesswrong.com", ".alignmentforum.org", ".effectivealtruism.org", "arbital.com"]

    mirrorPrefixes :: [String]
    mirrorPrefixes = ["www", "www", "ea", "arbital"]

    domainMapping :: [(String,String)]
    domainMapping = zip originalDomains mirrorPrefixes

    findMirrorPrefix :: String -> Maybe String
    findMirrorPrefix hostname = snd <$> find ((`isSuffixOf` hostname) . fst) domainMapping

    transformToMirrorURI :: String -> URI -> URIAuth -> URI
    transformToMirrorURI mirrorPrefix parsedURI auth =
        parsedURI { uriAuthority = Just $ auth { uriRegName = mirrorPrefix ++ ".greaterwrong.com" } }

    handleCommentId :: URI -> URI
    handleCommentId uri' =
        let query = parseQuery . C8.pack $ uriQuery uri'
        in case lookup "commentId" query of
            Just commentId -> uri' { uriPath = uriPath uri' ++ "/comment/" ++ C8.unpack (fromJust commentId)
                                  , uriQuery = C8.unpack $ renderQuery True $ delete ("commentId", commentId) query }
            Nothing -> uri'

    shouldTransform :: String -> String -> Bool
    shouldTransform hostname uri'' = isJust (findMirrorPrefix hostname) && not ("view=alignment-forum" `isInfixOf` uri'' && "www" `isPrefixOf` hostname)

-- redirect every WP to the mobile version, and if the top-level page is linked, append the ID of the top of the content to cut out the clutter:
-- > transformWPtoMobileWP "https://en.wikipedia.org/wiki/George_Washington"
-- → "https://en.​m.​wikipedia.org/wiki/George_Washington#bodyContent"
transformWPtoMobileWP :: String -> String
transformWPtoMobileWP uri = fromMaybe uri $ do
    parsedURI <- parseURI uri
    auth <- uriAuthority parsedURI
    let hostname = uriRegName auth
    if ".wikipedia.org" `isSuffixOf` hostname
        then return $ show $ parsedURI { uriAuthority = Just $ auth { uriRegName = modifyHostname hostname }, uriFragment = "#bodyContent" }
        else return uri
    where
        modifyHostname :: String -> String
        modifyHostname hostname = if ".m.wikipedia.org" `isInfixOf` hostname then hostname else replaceSuffix ".wikipedia.org" ".m.wikipedia.org" hostname
        replaceSuffix :: String -> String -> String -> String
        replaceSuffix old new s = if old `isSuffixOf` s then take (length s - length old) s ++ new else s

-- For Amazon links, there are two scenarios: there are parameters (denoted by a
-- '?' in the URL), or there are not. In the former, we need to append the tag as
-- another item ('&tag='), while in the latter, we need to set up our own
-- parameter ('?tag='). The transform may be run many times since
-- they are supposed to be pure, so we
-- need to also check a tag hasn't already been appended.
--
-- For non-Amazon links, we just return them unchanged.
addAmazonAffiliate :: String -> String
addAmazonAffiliate l = if ("www.amazon.com/" `isInfixOf` l) && not ("tag=gwernnet-20" `isInfixOf` l) then
                                        if "?" `isInfixOf` l then l ++ "&tag=gwernnet-20" else l ++ "?tag=gwernnet-20"
                                       else l

{- re URL transforms: Why?

The status quo of Arxiv links is unsatisfactory. Linking to the abstract page is the worst of all worlds. The
annotations for your standard Arxiv `/abs/` landing page link provide almost all of the information that the abstract page
does, and so the abstract page is redundant; the abstract page can't be popped-up either, as Arxiv is one of the many
websites which sets headers blocking cross-site loads so your web browser will refuse to pop it up. So an Arxiv link
requires at least 2 clicks to do anything useful: click on the title link, and then click on the `PDF` link. What to do?

We could use only PDF links. Arxiv `/abs/` links *could* be rewritten to go straight to `/pdf/`. The upside is that this
would make clicking on the link meaningful (saving 1 click), and it would integrate into my local archiving system
cleanly, so the link would be to a local mirror of the Arxiv PDF, which would be both faster & popup-able (saving 2
clicks while being nicer).
But the downside is that then mobile users will have a bad experience (it might need to download and be viewed in an
entirely separate app!) and people who don't want to deal with PDFs at that moment (eg. no night mode) would also prefer
to not be shunted into a PDF when they could have been linked to a HTML landing page. Thus, if you hang around Reddit or
Twitter or HN, you will see people or even outright bots responding to Arxiv PDF links with the `/abs/` instead. This
strikes me as fussy (I don't mind PDF links) but I can't deny that these people exist and seem to care.

Could we use non-abstract HTML links? Unlike BioRxiv/MedRxiv, where you can simply append `.full` and get a nice usable
HTML version, Arxiv provides only the PDF, so it's unclear what other HTML page you could send people to. The good news
is that there turns out there are projects to create HTML versions of Arxiv PDFs:
[Arxiv-vanity](https://www.arxiv-vanity.com/) and a new one, [Ar5iv](https://blog.arxiv.org/2023/12/21/accessibility-update-arxiv-now-offers-papers-in-html-format/). Both use the same
trick: a LaTeX→HTML compiler <https://github.com/brucemiller/LaTeXML>.
Compiling LaTeX to anything else is… hard. And many of the papers have
rendering problems, major or minor, or are not present at all. (Ar5iv is about a month out of date. They served an error
page before, but at my request changed it to redirect to Arxiv proper with the query parameter `fallback=original`
(redirecting without an option apparently confuses non-gwern.net readers), so you can just rewrite your Arxiv links and
not need to care about whether Ar5iv has it or not.) But they provide responsive reflowable HTML,
justification/hyphenation, and dark mode; so for a mobile smartphone user, this is, for many users and many papers and
many circumstances, better than your average 1-column paper, or constantly dragging on a 2-column paper. Still, it's not
*so* much better that all the PDF readers will want to see the HTML version instead of the PDF version…

So, linking to the `/abs/` makes no one happy; linking the PDF makes all mobile and some desktop users unhappy; and linking
to the Ar5iv HTML version is the opposite. What to do? Well, why not link *both*? Popups already have a system for
linking a local PDF or HTML archive of a URL, and also the URL: the PDF is the main link, and then a small `[LIVE]` link
is provided to the original live un-archived URL. So we could rewrite every `/abs/` link to `/pdf/`, which will then get
archived & rewritten to the local archive, and then the 'original' URL gets quietly rewritten Arxiv → Ar5iv. To make it
even more transparent, we swap 'LIVE' for 'HTML' (it's not really the 'live' link anymore, and 'HTML' tells the mobile
user it may serve them better.) Mobile users see the PDF icon, avoid it, and go to `[HTML]`, desktop or PDF-enjoyers
hover on it and without a click get their PDF, and after a bit of learning & adjustment (hopefully near-instant due to
the icons & text labels), everyone gets their preferred medium to read the paper.

The implementation is a little uglier than that because the popups JS code does not expect the original-URL data to be
fiddled with, but it works now and is live on Gwern.net.

While the logic is a little opaque to readers, I think this handles Arxiv much more cleanly than before. -}

------------------------------------------------------------------------------------------------------------------------

-- whitelist of strings/domains which are safe to link to directly, either because they have a long history of stability
-- & reader-friendly design, or attempting to archive them is pointless (eg. interactive services); and blacklist of
-- URLs we always archive even if otherwise on a safe domain:
-- 1. some matches we always want to skip
-- 2. after that, we want to mirror PDFs everywhere (except Gwern.net because that's already 'mirrored')
-- 3. after that, we may want to skip various filetypes and domains
whiteList :: String -> Bool
whiteList url
  | anyInfix url ["citeseerx.ist.psu.edu"] = False -- TODO: after fixing all existing CiteSeerx links, set this rule to False
  | anyPrefix url ["/", "./", "../", "https://gwern.net", "#", "!", "$", "mailto", "irc", "/metadata/", "/doc/"] = True
  | anySuffix url [".pdf", "/pdf", ".pdf#"] = False
  | anyInfix url whiteListMatchesFixed = True
    | otherwise = False

-- TODO: refactor into URLs vs regexps vs domains (so they can be validated & tested)
-- TODO: begin removing 'stable' because now we reuse local-archives for file-transcludes and so there's a reason to make local-archives beyond avoiding linkrot
whiteListMatchesFixed :: [String]
whiteListMatchesFixed = [
      "archive.org/details/", "archive.org/download/", "scholar.archive.org"
      , ".txt" -- TODO: generalize the PDF download to handle all non-HTML filetypes
      , ".xlsx"
      , ".xz"
      , ".csv"
      , ".ps"
      , ".mp3"
      , ".png"
      , ".ogg"
      , ".jpg"
      -- , "distill.pub" -- interactive?
      -- , "thegradient.pub" -- interactive?
      , "bakabt.me" -- login
      , "girls.moe" -- interactive (DL model)
      , "wikimedia.org" -- WP: handled by specialcase popups
      , "wikipedia.org" -- WP: handled by specialcase popups
      , "r-inla.org"
      , "coursera.org"
      , "ourworldindata.org"
      , "familysearch.org"
      , "rcpsych.org"
      , "jneurosci.org"
      , "mediawiki.org"
      , "emacswiki.org"
      , "dwarffortresswiki.org"
      , "ledger-cli.org"
      , "doi.org"
      , "mnemosyne-proj.org"
      , "imagemagick.org"
      , "onegeek.org"
      , "kk.org"
      , "econtalk.org"
      , "deeplearningbook.org"
      , "openscienceframework.org"
      , "equator-network.org"
      , "haskell.org"
      , "realworldhaskell.org"
      , "tryhaskell.org"
      , "givewell.org"
      , "archiveteam.org"
      , "nejm.org"
      , "longform.org"
      , "qntm.org"
      , "4chan.org"
      , "debian.org"
      , "mc-stan.org"
      , "ajcn.org"
      , "libgen.org"
      , "frontiersin.org"
      , "journalofvision.org"
      , "wikimediafoundation.org"
      , "poetryfoundation.org"
      , "psychiatryinvestigation.org"
      , "scitation.org"
      , "rdocumentation.org"
      , "ama-assn.org"
      , "datascienceassn.org"
      , "wikichip.org"
      , "escholarship.org"
      , "winehq.org"
      , "semanticscholar.org"
      , "tools.wmflabs.org"
      , "yourmorals.org"
      , "tasvideos.org"
      , "philpapers.org"
      , "r-project.org"
      , "metafor-project.org"
      , "torproject.org"
      , "personality-project.org"
      , "jstatsoft.org"
      , "hathitrust.org"
      , "safebooru.org"
      , "derpibooru.org"
      , "usenix.org"
      , "scipy.org"
      , "wiktionary.org"
      , "wellcomelibrary.org"
      , "ecologyandsociety.org"
      , "ieee-security.org"
      , "jwz.org"
      , "google.com/search"
      , "vast.ai"
      , "iqtest.dk"
      , "ac.uk"
      , "goo.gl"
      , "dtic.mil"
      , "fastmail.fm"
      , "c2.com"
      , "everything2.com"
      , "doc88.com"
      , "arstechnica.com" -- stable / poor-quality archiving - articles are usually split over multiple pages, so archiving just the link is no good
      , "uptontea.com"
      , "wolframalpha.com"
      , "nvidia.com"
      , "wikia.com"
      , "algolia.com"
      , "atlasobscura.com"
      , "gitlab.com"
      , "flashcarddb.com"
      , "imdb.com"
      , "sagepub.com"
      , "amsciepub.com"
      , "liebertpub.com"
      , "bbc.com"
      , "theatlantic.com"
      , "xkcd.com"
      , "buzzfeed.com"
      , "cogmed.com"
      , "haveibeenpwned.com"
      , "ted.com"
      , "deepmind.com"
      , "wunderground.com"
      , "soundcloud.com"
      , "youtube.com"
      , "sendspace.com"
      , "vice.com"
      , "googlecode.com"
      , "paperswithcode.com"
      , "flashcardexchange.com"
      , "stackexchange.com"
      , "google-melange.com"
      , "theverge.com"
      , "kaggle.com"
      , "bigquery.cloud.google.com"
      , "cse.google.com"
      , "translate.google.com"
      , "drive.google.com"
      , "colab.research.google.com" -- highly interactive/complex
      , "powersearchingwithgoogle.com"
      , "scholar.google.com"
      , "docs.google.com"
      , "spreadsheets.google.com"
      , "plus.google.com"
      , "tandfonline.com"
      , "sciencenewsline.com"
      , "examine.com"
      , "discovermagazine.com"
      , "tomshardware.com"
      , "joelonsoftware.com"
      , "mediafire.com"
      , "theuncertainfuture.com"
      , "timeanddate.com"
      , "iqcomparisonsite.com"
      , "mail-archive.com"
      , "darknetlive.com"
      , "docsdrive.com"
      , "tineye.com"
      , "bloomberg.com"
      , "patch.com"
      , "dual-n-back.com"
      , "clickotron.com"
      , "amazon.com" -- service
      , "amzn.to" -- service redirect
      , "amzn.com"
      , "vimeo.com" -- video, need yt-dl or similar
      , "duckduckgo.com"
      , "duolingo.com"
      , "bandcamp.com"
      , "artbreeder.com" -- interactive/service
      , "beeminder.com" -- interactive/service
      , "pcpartpicker.com" -- interactive
      , "talktotransformer.com" -- interactive
      , "feedburner.com" -- service
      , "kickstarter.com" -- service
      , "kill-the-newsletter.com" -- service
      , "flickr.com" -- stable
      , "cryonicscalculator.com" -- interactive/service
      , "waifulabs.com" -- interactive/service
      , "rpubs.com"
      , "cambridgebrainsciences.com"
      , "googleapis.com"
      , "goproblems.com"
      , "r-bloggers.com"
      , "thepharmacyexpress.com" -- interactive/service
      , "inklingmarkets.com" -- interactive/service
      , "academictorrents.com" -- interactive/service
      , "dropboxusercontent.com" -- needs manual archiving
      , "millionshort.com" -- service
      , "cogtest.com" -- service
      , "iqtest.com" -- service
      , "proquest.com" -- needs manual archiving
      , "thesecatsdonotexist.com" -- interactive/service
      , "thiseyedoesnotexist.com" -- interactive/service
      , "thismarketingblogdoesnotexist.com" -- interactive/service
      , "thisrentaldoesnotexist.com" -- interactive/service
      , "thisvesseldoesnotexist.com" -- interactive/service
      , "thispersondoesnotexist.com" -- interactive/service
      , "thiscatdoesnotexist.com" -- interactive/service
      , "thisstorydoesnotexist.com" -- interactive/service
      , "thiswaifudoesnotexist.net" -- interactive/service
      , "thisanimedoesnotexist.ai" -- interactive/service
      , "iqout.com"  -- interactive/service
      , "psyarxiv.com" -- stable
      , "stackroboflow.com"
      , "wikiwix.com" -- archive/mirror
      , "dropbox.com" -- needs manual archiving
      , "dl.dropbox.com" -- needs manual archiving
      , "gratipay.com" -- dead
      , "otokei-dou.in" -- dead
      , "bl3j73taluhwidx5.onion" -- dead
      , "thehub7dnl5nmcz5.onion" -- dead
      , "silkroad5v7dywlc.onion" -- dead
      , "thehub7gqe43miyc.onion" -- dead
      , "hmxutiksabsj4ozi.onion" -- dead
      , "5onwnspjvuk7cwvk.onion" -- dead
      , "grams7enufi7jmdl.onion" -- dead
      , "assmkedzgorodn7o.onion" -- dead
      , "silkroadvb5piz3r.onion" -- dead
      , "lacbzxobeprssrfx.onion" -- dead
      , "alpaca727o3c75xx.onion" -- dead
      , "kth2mwuwlkezwziy.onion" -- dead
      , "i25c62nvu4cgeqyz.onion" -- dead
      , "gitcoin.co"
      , "gitlab.io" -- stable
      , "libgen.io" -- archive
      , "websitedownloader.io" -- service/source code
      , "webrecorder.io" -- service/source code
      , "keras.io" -- stable
      , "shinyapps.io" -- interactive
      , "archivebox.io" -- archive
      , "toranoana.jp" -- click-walled
      , "intrade.com/jsp" -- dead
      , "archive.is" -- archive
      , "books.google.com/books"
      , "reddit.com/r/AgMarketplace/" -- dead (banned)
      , "reddit.com/r/BlackBank/" -- dead (banned)
      , "reddit.com/r/DarkNetDeals/" -- dead (banned)
      , "reddit.com/r/DarkNetMarkets/" -- dead (banned)
      , "reddit.com/r/DarknetMarketsNZ/" -- dead (banned)
      , "reddit.com/r/MEMarketplace/" -- dead (banned)
      , "reddit.com/r/QuantikXanax/" -- dead (banned)
      , "reddit.com/r/blackmarketreloaded/" -- dead (banned)
      , "reddit.com/r/fakeid/" -- dead (banned)
      , "reddit.com/r/grams/" -- dead (banned)
      , "reddit.com/r/havanamarket/" -- dead (banned)
      , "reddit.com/r/medsforbitcoins/" -- dead (banned)
      , "reddit.com/r/modup/" -- dead (banned)
      , "reddit.com/r/sanitariummarket/" -- dead (banned)
      , "reddit.com/r/themarketplace/" -- dead (banned)
      , "donmai.us" -- stable/NSFW
      , "anidb.net" -- stable
      , "ankiweb.net"
      , "researchgate.net" -- needs to be replaced by hand
      , "nearlyfreespeech.net" -- service
      , "epjournal.net"
      , "gwern.net" -- redundant
      , "videolectures.net" -- service/interactive
      , "ankisrs.net" -- stable
      , "cloudfront.net" -- service
      , "obormot.net" -- stable
      , "personalitytest.net" -- interactive
      , "myanimelist.net" -- stable
      , "mathoverflow.net" -- stable
      , "http://get.tt/" -- archive/dead
      , "academia.edu" -- need to be replaced by hand
      , "ndcourts.gov" -- interactive/service
      , "uscourts.gov" -- interactive/service
      , "treasurydirect.gov" -- service
      , "twitch.tv"  -- service
      , "archive.today" -- archive
      , "archive.ph" -- archive
      , "quantum.country" -- interactive
      , "numinous.productions" -- stable/interactive
      , "neuralnetworksanddeeplearning.com" -- stable
      , "mega.nz"  -- service
      , "stylegan.xyz" -- dead
      , "beepb00p.xyz" -- stable
      , "equestriadaily.com/search" -- service
      , "libgen.is" -- service
      , "libgen.rs" -- service
      , "nyaa.se" -- service
      , "nyaa.eu" -- service
      , "nyaa.si" -- service
      , "modup.net" -- service
      , "flutterguy.org" -- service
      , "patreon.com/" -- service
      , "ricon.dev" -- service
      , "kanotype.iptime.org:8003" -- service (DeepDanbooru)
      , "sourceforge.net" -- service/source code
      , "waifu2x.udp.jp" -- service
      , "danwang.co" -- stable
      , "cognitivefun.net" -- interactive
      , "groups.google.com" -- low quality
      , "www.impactcybertrust.org" -- stable/service
      , "old.reddit.com/r/DarkNetMarkets/" -- dead (banned) deleted by Reddit
      , "old.reddit.com/r/afinil/" -- over-18 so logged out SingleFile doesn't work
      , "old.reddit.com/r/SilkRoad/" -- over-18 so logged out SingleFile doesn't work
      , "reddit.com/r/gwern/.rss" -- news/portal
      , "antilop.cc" -- stable
      , "www.ncbi.nlm.nih.gov" -- stable
      , "eric.ed.gov" -- stable
      , "scholars-stage.org" -- stable
      , "toolbox.google.com/datasetsearch/" -- interactive
      , "trends.google.com" -- interactive
      , "landing.google.com/sre" -- stable
      , "onlinelibrary.wiley.com" -- low-quality archives
      , "thepiratebay." -- service
      , "reddit.com/user/gwern/" -- news/portal
      , "old.reddit.com/r/gwern/" -- private
      , "snpedia.com" -- stable
      , "recapthelaw.org" -- service
      , "dspace.mit.edu" -- stable
      , "www.find-more-books.com/" -- service
      , "www.abebooks.com/" -- service
      , "www.thriftbooks.com/" -- service
      , "www.betterworldbooks.com/" -- service
      , "www.barnesandnoble.com/" -- service
      , "www.discoverbooks.com" -- service
      , "camelcamelcamel.com/" -- service
      , "1dollarscan.com/" -- service
      , "cryptome.org" -- stable
      , "danluu.com" -- stable
      , "getlamp.com" -- stable
      , "vizhub.healthdata.org" -- interactive
      , "ai.googleblog.com" -- way too heavy
      , "blog.otoro.net" -- low quality: can't guarantee animations/interactions work via SingleFile
      , "quora.com" -- low quality: so heavily paywalled/blocked no point
      , "casual-effects.com" -- service/source code
      , "pcdb.santafe.edu" -- updated database
      , "unesdoc.unesco.org" -- low quality
      , "fifteen.ai/" -- service
      , "cran.r-project.org" -- stable
      , "mattlakeman.org" -- stable? we'll give him a chance
      , "gallica.bnf.fr" -- low quality/interactive
      , "find-more-books.com" -- service
      , "nicovideo.jp" -- service/videos
      , "darwin-online.org.uk" -- interactive documents
      , "ncase.me" -- interactive
      , "facebook.com" -- low quality (too JS-heavy, requires log-in etc)
      , "bellroy.com"
      , "mruniversity.com" -- videos
      -- , "imgur.com"
      , "proofofexistence.com" -- service
      , "sifter.org" -- service
      , "newegg.com" -- service
      , "worrydream.com" -- interactive/stable
      , "thiscardoesnotexist.glitch.me" -- service
      , "windowsphone.com" -- service/low quality
      , "ohyouprettythings.com" -- service
      , "aurellem.org" -- low quality (YouTube links don't inline properly)
      , "stlouisfed.org" -- service/stable (FRED economic statistics)
      , "apps.allenai.org" -- service/interactive
      , "bam-dataset.org" -- service/dataset
      , "blockchain.info" -- service
      , "polecat.mascherari.press" -- dataset
      , "explorabl.es" -- interactive
      , "papers.ssrn.com" -- already blacklisted for manual archiving
      , "thisfursonadoesnotexist.com" -- interactive
      , "plato.stanford.edu" -- stable/bad archiving
      , "harney.com" -- service
      , "press.etc.cmu.edu" -- downloads/service
      , "tom7.org/mario/" -- stable
      , "orbis.stanford.edu" -- interactive
      , "www-cs-faculty.stanford.edu/~knuth/" -- stable
      , "www.cylab.cmu.edu" -- stable
      , "bibliophilly.library.upenn.edu" -- interactive
      , "fis.fda.gov" -- interactive
      , "www.genome.gov" -- stable
      , "dantelabs.com" -- service
      , "www.cdc.gov/nchs" -- service/data
      , "www.cdc.gov/ncbddd/" -- service/data
      , "folding.stanford.edu" -- stable
      , "www.writingroutines.com" -- stable/updating
      , "wolfewiki.com" -- stable
      , "www.vocativ.com/interactive/" -- interactive
      , "row1.ca" -- bad archiving?
      , "folkrnn.org" -- stable
      , "flatisjustice.moe/TADNE" -- service
      , "thecleverest.com" -- service
      , "inventingabstraction.tumblr.com" -- bad archiving
      , "www.moma.org" -- bad archiving
      , "www.e-codices.unifr.ch" -- bad archiving
      , "darkdata.bc.edu" -- interactive
      , "thisponydoesnotexist.net" -- interactive
      , "eurekamag.com" -- service
      , "rationality.org" -- stable
      , "www.thisworddoesnotexist.com" -- service
      , "substack.com" -- service
      , "gwern.substack.com" -- service
      , "jasoncrawford.org" -- stable
      , "progressstudies.school" -- service
      , "readonlymemory.vg" -- service
      , "hivemind-repo.s3-us-west-2.amazonaws.com/twdne3/" -- large binaries
      , "fursona.app" -- redirect to Google Colab
      , "old.reddit.com/r/reinforcementlearning/search" -- service
      , "usesthis.com" -- stable
      , "modafinil-store.com" -- dead
      , "https://huggingface.co/spaces/teven-projects/calculator" -- interactive
      , "sevensecularsermons.org" -- stable
      , "listudy.org" -- service
      , "playground.tensorflow.org" -- interactive
      , "ciechanow.ski" -- interactive, JS widgets didn't preserve right
      , "serendipityrecs.com" -- service
      , "discord.gg" -- service
      , "gptprompts.wikidot.com" -- updated/stable
      , "leaderboard.allenai.org" -- updated/low-quality archiving
      , "cdm16630.contentdm.oclc.org" -- low-quality archiving
      , "ergodox-ez.com" -- service/sales
      , "archive.foolz.us" -- blocks crawlers
      , "arxiv-vanity.com" -- mirror of stable site
      , "yalelawjournal.org" -- low-quality archiving (sidenotes don't work)
      , "www.theindiaforum.in" -- low-quality archiving (sidenotes don't work)
      , "knightcolumbia.org" -- low-quality archiving (sidenotes don't work)
      , "thefirstaibook.com" -- low-quality archiving
      , "tl.net/blogs/" -- low-quality archiving (screenshots/images break)
      , "metaculus.com" -- updated/service/stable
      , "simplify.so" -- service
      , "old.reddit.com/domain/gameprogrammingpatterns.com/" -- service/updated
      , "ctan.org/pkg/" -- stable
      , "magicemail.io" -- service
      , "parametric.press/issue-01/unraveling" -- doesn't archive the interactive right
      , "vision-explorer.allenai.org/" -- interactive service
      , "aidungeon.io" -- updated/interactive service
      , "www.tensorflow.org" -- stable/updated (technical documentation, better not point at potentially-outdated archives)
      , "bit-player.org" -- low-quality (interactive JS widgets didn't preserve right)
      , "www.themoneyillusion.com" -- low-quality
      , "cedar.wwu.edu/cedarbooks/4/" -- stable
      , "krebsonsecurity.com" -- stable
      , "buttercupfestival.com" -- stable
      , "www.cna.org" -- landing/homepage, not useful to archive
      , "www.caniuse.com" -- updated service
      , "archiveofourown.org" -- blocks archiving
      , "flashgamehistory.com" -- low quality
      , "thebrowser.com" -- paywall
      , "git.sr.ht" -- service/updated
      , "b-ok.cc/" -- service
      , "www.nap.edu/catalog" -- low quality
      , "store.steampowered.com/app/" -- service
      , "www.traditionalmedicinals.com/" -- homepage
      , "hiyo.jp" -- service/defunct
      , "wkhtmltopdf.org" -- stable
      , "www.sumatrapdfreader.org" -- stable
      , "tradejournalcooperative.com" -- service
      , "www.openphilanthropy.org" -- stable
      , "alignmentforum.org" -- stable
      , "forum.effectivealtruism.org" -- stable
      , "www.effectivealtruism.org" -- stable
      , "www.preclinicaltrials.eu" -- service
      , "disease-connect.org" -- service
      , "chrome.google.com/webstore/" -- service
      , "microsoftedge.microsoft.com/addons/" -- service
      , "magenta.tensorflow.org/music-transformer" -- low quality
      , "magenta.tensorflow.org/maestro-wave2midi2wave" -- low quality
      , "magenta.tensorflow.org/piano-transformer" -- low quality
      , "e621.net" -- service/porn
      , "www.ctan.org" -- stable
      , "ideas.repec.org/p/uea/" -- stable (preprint server)
      , "freakonomicsexperiments.com" -- service
      , "apps.chiragjpgroup.org/catch/" -- service (interactive stats)
      , "fdaaa.trialstracker.net/" -- service (interactive stats)
      , "rottenlibrary.net" -- redundant (mirror of my rotten.com mirror)
      , "www.justinpinkney.com" -- stable
      , "ssb.fit" -- service
      , "course.fast.ai/videos/" -- low quality
      , "vk.com/alexeyguzey" -- stable
      , "paulfchristiano.com" -- stable
      , "ought.org" -- homepage
      , "theory.cs.berkeley.edu/" -- homepage
      , "ai-alignment.com" -- stable?
      , "sideways-view.com" -- stable
      , "jakewestfall.org/ivy/" -- interactive
      , "findclone.ru" -- service
      , "reddit.com/user/shawwwn" -- updated/profile
      , "www.projectvesta.org" -- homepage
      , "pygments.org" -- homepage
      , "test.mensa.no" -- service
      , "www.queendom.com/tests/" -- service
      , "brainturk.com" -- service
      , "www.courtlistener.com/" -- stable
      , "console.cloud.google.com" -- cloud service requiring login
      , "imsdb.com" -- homepage
      , "www.reg.ru/blog/anime-generation-with-stylegan/" -- unnecessary (mirror of /face)
      , "catscan2.toolforge.org" -- interactive/service
      , "www.mangafox.com/manga" -- service/pirated
      , "highlightcam.co.nz" -- homepage
      , "michaelnielsen.org" -- stable/homepage
      , "juliagalef.com" -- stable/homepage
      , "opensnp.org" -- service/database
      , "tzstamp.io" -- service
      , "ggplot2.tidyverse.org" -- homepage
      , "fantasticanachronism.com" -- stable
      , "coderelay.io/fontemon.html" -- interactive
      , "www.speedtest.net" -- service
      , "okmij.org" -- stable
      , "packdeps.haskellers.com" -- service
      , "meyerweb.com/eric/tools/" -- service
      , "bifunctor.homelinux.net" -- service
      , "paste.hskll.org" -- service
      , "tuftandneedle.com" -- homepage
      , "www.microcovid.org" -- service
      , "grantland.com/features/" -- stable
      , "instant.md" -- stable
      , "support.google.com" -- updated
      , "competicionmental.appspot.com" -- app/game/interactive
      , "lair.lighton.ai/akronomicon/" -- updated
      , "public.tableau.com" -- interactive visualizations
      , "andyljones.com" -- stable? give a chance
      , "mlcommons.org" -- organization/homepage
      , "eleuther.ai" -- stable
      , "ieeexplore.ieee.org/abstract/" -- bad quality
      , "arankomatsuzaki.wordpress.com" -- stable
      , "www.fimfiction.net" -- stable/homepage
      , "gaussianbp.github.io" -- interactive
      , "bitcoinbook.cs.princeton.edu" -- homepage
      , "theme.typora.io" -- stable/updated source code/docs
      , "sigbovik.org/" -- stable/homepage
      , "sociologicalscience.com" -- stable (preprint host)
      , "eugenewei.com" -- stable
      , "h01-dot-neuroglancer-demo.appspot.com" -- interactive
      , "dawn.cs.stanford.edu/benchmark/" -- updated database/benchmark
      , "super.gluebenchmark.com" -- updated database/benchmark
      , "lab.aminer.cn" -- interactive/demo
      , "www.rte.ie/archives/" -- video
      , "geroprotectors.org" -- updated database
      , "www.satcompetition.org" -- updated database
      , "endlessvn.io/" -- homepage
      , "readthedocs.io" -- generally stable, updated docs
      , "blog.codinghorror.com" -- stable
      , "teanobi.com" -- homepage
      , "scp-wiki.wikidot.com" -- stable/updated
      , "programmablesearchengine.google.com" -- service
      , "library.bz/" -- Libgen
      , "wellcomecollection.org/works/" -- low-quality
      , "www.reddit.com/r/mlscaling/search" -- search/updated
      , "thechineseteashop.com/" -- homepage
      , "teahabitat.com/" -- homepage
      , "raganwald.com" -- stable
      , "aclanthology.org" -- stable
      , "ml.berkeley.edu/blog/" -- low-quality - redirects?
      , "advertising-effects.chicagobooth.edu/" -- interactive
      , "carbonplan.org/research/forest-offsets" -- interactive
      , "www.avclub.com/the-100-best-worst-and-weirdest-things-we-saw-on-the-1839566367" -- too much media
      , "https://ww2.arb.ca.gov/our-work/programs/cap-and-trade-program" -- homepage
      , "https://egamebook.com/lochness/" -- interactive
      , "https://sites.research.google/trc/about/" -- homepage
      , "https://realbotix.com/" -- homepage
      , "https://www.grillitype.com/" -- homepage
      , "https://www.royalroad.com/fiction/" -- stable
      , "https://www.lipreading.org/" -- homepage
      , "https://research.google/pubs/" -- stable
      , "https://www.tabnine.com/" -- homepage
      , "https://www.alphagomovie.com/" -- stable
      , "https://pone.dev/" -- interactive
      , "https://www.luckyscent.com/" -- homepage
      , "https://pioneer.app/" -- homepage
      , "dev.kanotype.net:8003/deepdanbooru/" -- interactive/service
      , "https://app.inferkit.com/demo"  -- interactive/service
      , "https://animetudes.com" -- low quality (media)
      , "https://icar-project.com/" -- homepage
      , "https://discord.com/invite/" -- service
      , "https://same.energy/" -- interactive/service
      , "https://www.reddit.com/r/MediaSynthesis/" -- low-quality due to Imgur/image embeds
      , "proceedings.mlr.press/" -- stable
      , "https://absa.org/" -- homepage
      , "https://www.agriapet.co.uk/" -- homepage
      , "www.word.golf" -- interactive game
      , "https://demo.allennlp.org" -- interactive demos
      , "https://www.cancerresearchuk.org/about-us" -- homepage
      , "mymodafinil.net" -- dead
      , "https://free.law/recap" -- homepage
      , "https://dominicfrisby.com/" -- homepage
      , "https://forum.effectivealtruism.org" -- stable
      , "https://bwc.thelab.dc.gov/" -- low quality
      , "https://www.trubrain.com/" -- homepage
      , "https://energycontrol.org/quienes-somos/proyectos/" -- homepage
      , "https://www.teaandabsinthe.com/" -- homepage
      , "https://www.limeadery.com/" -- homepage
      , "https://taooftea.com/" -- homepage
      , "https://ooo.ghostbows.ooo/" -- interactive/sound
      , "https://www.jetbrains.com/lp/mono/" -- low quality
      , "linusakesson.net" -- low quality (YT embed breaks)
      , "https://willcrichton.net/nota/" -- interactive
      , "https://trixter.oldskool.org/" -- low quality (YT embed breaks)
      , "http://www.michaelburge.us/2019/05/21/marai-agent.html" -- low quality (YT embed breaks)
      , "https://pol.is/home" -- homepage
      , "http://images.google.com/images" -- interactive (search engine query)
      , "https://danijar.com/project/apd/" -- video embed
      , "http://lispm.de/symbolics-lisp-machine-ergonomics" -- video embed
      , "https://boardgamegeek.com" -- stable
      , "https://dl.acm.org/doi/10.1145/3266037.3266090" -- Open Access
      , "https://laion.ai/laion-400-open-dataset/" -- video embed
      , "https://wy-lang.org/" -- homepage
      , "https://ali-design.github.io/gan_steerability/" -- video embed
      -- , "https://github.com/" -- stable
      , "https://xbpeng.github.io/projects/VDB/index.html" -- video embed
      , "https://news.ycombinator.com/newest" -- updated
      , "https://news.ycombinator.com/news" -- updated
      , "http://www.packomania.com/" -- low quality/updated
      , "https://metarationality.com/rational-pcr" -- video embed
      , "https://ashish-kmr.github.io/rma-legged-robots/" -- video embed
      , "https://www.clinicaltrialsregister.eu/ctr-search/search" -- interactive
      , "https://imagelibrary.bgu.ac.il/pf.tlx/O6ORSOx-nut" -- already mirrored locally
      , "https://models.aminer.cn/CogView/index.html" -- interactive
      , "https://tfhub.dev/google/collections/gtr/1" -- source code/doc/updated
      , "https://universome.github.io/stylegan-v" -- video embed
      , "https://openaipublic.blob.core.windows.net/webgpt-answer-viewer/index.html" -- interactive
      , "http://recur-env.eba-rm3fchmn.us-east-2.elasticbeanstalk.com/" -- interactive
      , "https://pandoc.org/" -- homepage
      , "https://caniuse.com/" -- updated
      , "https://www.vesta.earth/" -- homepage
      , "https://www.janelia.org/project-team/flyem/hemibrain" -- video embeds
      , "https://seegrid.com/" -- homepage
      , "https://www.sixdegreesofwikipedia.com/" -- interactive
      , "https://rdiff-backup.net/" -- homepage
      , "https://conifer.rhizome.org/" -- homepage
      , "https://manifold.markets/" -- homepage
      , "https://highnoongmt.wordpress.com/" -- homepage
      , "https://fanfox.net/manga/oyasumi_punpun/v08/c084/15.html" -- blocks mirroring
      , "https://mru.org/development-economics" -- video embeds
      , "https://www.pluralsight.com/" -- interactive
      , "https://www.sapa-project.org/" -- homepage
      , "https://www.cram.com/topics/popular" -- updated
      , "https://www.cram.com" -- homepage
      , "https://knowyourmeme.com/memes/" -- stable/updated
      , "https://wellcomecollection.org/" -- low quality
      , "https://cognitivemedium.com/" -- stable
      , "https://arxiv.org/abs/quant-ph/" -- these really old pre-2007 (https://arxiv.org/help/arxiv_identifier) Arxiv papers don't work with Ar5iv, so we'll exempt them from the archive+rewrite process
      , "https://arxiv.org/abs/math/"
      , "https://arxiv.org/abs/physics/"
      , "https://arxiv.org/abs/cond-mat/"
      , "https://arxiv.org/abs/cs/"
      , "https://sebastianrisi.com/self_assembling_ai/" -- video embeds
      , "https://brainwindows.wordpress.com/2009/10/14/playing-quake-with-a-real-mouse/" -- video embed
      , "https://medium.com/mindsoft/rats-in-doom-eb6c52c73aca" -- video embed
      , "https://podcasts.google.com/feed/aHR0cHM6Ly9yc3MuYWNhc3QuY29tL2Rhbm55aW50aGV2YWxsZXk/episode/MDI4NDI4ODMtZmE3YS00MzA2LTk1ZGItZjgzZDdlMzAwZThk" -- audio embed
      , "https://jdlm.info/articles/2018/03/18/markov-decision-process-2048.html" -- interactive solver
      , "https://www.dailydot.com/crime/hive-silk-road-online-drug-culture-history/" -- video embed
      , "browse.arxiv.org" -- HTML mirror of Arxiv PDFs, regularly updated to improve compilation quality, and allows live popups so no need to mirror locally
      , "digital.library.unt.edu/explore/partners/" -- search
      , "worldcat.org" -- updated
      , "tinyurl.com" -- redirects
      , "www.character.ai" -- homepage
      , "ageofem.com" -- low quality (video embeds)
      , "https://mujoco.org/" -- homepage
      , "https://sites.google.com/view/mbmf" -- low quality (video embeds)
      , "https://pathak22.github.io/large-scale-curiosity/" -- low quality (video embeds)
      , "https://adversarialpolicies.github.io/" -- low quality (video embeds)
      , "https://svito-zar.github.io/gesticulator/" -- low quality (video embeds)
      , "https://pearl-insertion.github.io/" -- low quality (video embeds)
      , "https://ramanans1.github.io/plan2explore/" -- low quality (video embeds)
      , "https://wilson1yan.github.io/videogpt/index.html" -- low quality (video embeds)
      , "https://repurposegans.github.io/" -- low quality (video embeds)
      , "https://adapterhub.ml/" -- homepage
      , "https://www.megaverse.info/" -- low quality (video embeds)
      , "https://attentionneuron.github.io/" -- interactive
      , "https://es-clip.github.io/" -- low quality (video/animated embeds)
      , "https://sites.google.com/view/mbrl-amortization/home" -- low quality (video embeds)
      , "https://sites.google.com/view/mend-editing" -- low quality (animated embeds)
      , "https://energy-based-model.github.io/comet/" -- low quality (video embeds)
      , "https://huggingface.co/spaces/" -- interactive
      , "https://www.arknights.global/" -- homepage
      , "http://arborjs.org/" -- homepage
      , "https://songweige.github.io/projects/tats/index.html" -- video
      , "https://self-distilled-stylegan.github.io/" -- low quality (video embeds)
      , "https://www.flavorwire.com/415737/5-of-the-most-scandalous-affairs-in-literary-history" -- fails to archive, as does https://story.californiasunday.com/cosmic-crisp-apple-launch/ https://www.outsideonline.com/health/training-performance/inside-look-surprisingly-violent-quidditch-world-cup/ https://www.technologyreview.com/2020/02/17/844721/ai-openai-moonshot-elon-musk-sam-altman-greg-brockman-messy-secretive-reality/
      , "https://semantle.com/" -- interactive game
      , "https://agc.platform.baai.ac.cn/CogView/index.html" -- interactive demo
      , "https://unminify.com/" -- interactive service
      , "https://www.bulletproof.com/" -- homepage
      , "https://www.google.com/alerts" -- interactive service
      , "http://darwintunes.org/" -- SoundCloud music sound embed doesn't preserve
      , "https://danlark.org/2022/04/20/changing-stdsort-at-googles-scale-and-beyond/" -- low quality: video embeds
      , "http://relative-attention-music-nips-2018.s3-website-us-west-1.amazonaws.com/" -- low quality: sound embeds
      , "https://sites.google.com/view/nips2017-one-shot-imitation/home" -- low quality (video embeds)
      , "https://www.flickchart.com/Splash.aspx?return=%2f" -- interactive service/homepage
      , "https://sites.google.com/view/hybridmrca" -- low quality (video embeds)
      , "https://roboturk.stanford.edu/realrobotdataset" -- low quality (video embeds)
      , "https://wenlong.page/modular-rl/" -- low quality (video embeds)
      , "https://retinagan.github.io/" -- low quality (entire page broken?)
      , "https://speechresearch.github.io/deepsinger/" -- low quality (sound embeds)
      , "https://sites.google.com/view/efficient-robotic-manipulation" -- low quality (video embeds)
      , "https://next-week-tonight.github.io/NWT/" -- low quality (video embeds)
      , "https://magenta.tensorflow.org/perceiver-ar" -- low quality (audio embeds)
      , "https://hazyresearch.stanford.edu/sashimi-examples/" -- low quality (audio embeds)
      , "https://speechresearch.github.io/naturalspeech/" -- low quality (audio embeds)
      , "https://keithito.com/LJ-Speech-Dataset/" -- low quality (audio embeds)
      , "https://carolineec.github.io/informative_drawings/" -- low quality (video embeds)
      , "https://parti.research.google/" -- low-quality (initial image carousel doesn't work)
      , "https://unconventionality.github.io/" -- low-quality (interactive app breaks)
      , "https://elicit.com/" -- homepage
      , "https://yzqin.github.io/dexmv/" -- low-quality (video, GIF embeds break)
      , "https://say-can.github.io/" -- low quality (video embeds)
      , "https://wenlong.page/language-planner/" -- low quality (video embeds)
      , "https://patents.google.com/?inventor" -- interactive
      , "https://sites.google.com/view/ving-robot" -- low quality (video embeds)
      , "https://agility.csail.mit.edu/" -- low quality (video embeds)
      , "https://plai.cs.ubc.ca/2022/05/20/flexible-diffusion-modeling-of-long-videos/" -- low quality (video embeds)
      , "https://models.aminer.cn/cogvideo/" -- interactive service
      , "https://danijar.com/project/director/" -- low quality (video embeds)
      , "https://bigvgan-demo.github.io/" -- low quality (audio embeds)
      , "https://gist.github.com/brockmanmatt/7265297f21634693868c2aad9d2c5919" -- Github iPython notebook - always fail to render for me
      , "https://gist.github.com/brockmanmatt/deafb4dba7e4399327e44f2c8fd97b2b" -- Github iPython notebook - always fail to render for me
      , "https://sites.google.com/berkeley.edu/fleet-dagger/home" -- low quality (video embeds)
      , "https://danijar.com/project/daydreamer/" -- low quality (video embeds)
      , "https://sites.google.com/view/lmnav" -- low quality (video embeds)
      , "https://innermonologue.github.io/" -- low quality (video embeds)
      , "https://salu133445.github.io/mtmt/" -- low quality (audio embeds)
      , "https://paddlehelix.baidu.com/app/drug/protein" -- interactive
      , "https://semantic-abstraction.cs.columbia.edu/" -- low quality (video embeds)
      , "https://nuwa-infinity.microsoft.com/" -- low quality (video embeds)
      , "https://celebv-hq.github.io/" -- low quality (video embeds)
      , "https://baghunter.com/" -- homepage
      , "https://www.talkrl.com/episodes/" -- low quality (audio embeds)
      , "https://www.ultimagenomics.com/" -- homepage
      , "https://google.github.io/deepmac/" -- low quality (video embeds)
      , "https://semantle.pimanrul.es/" -- interactive
      , "https://labelerrors.com/" -- interactive
      , "https://novelai.net/" -- homepage
      , "https://well-typed.com/" -- homepage
      , "https://everynoise.com/engenremap.html" -- interactive
      , "https://jamiepinheiro.com/cyclic_tv_reference_paradox_finder/" -- interactive
      , "https://practiceoftheory.weebly.com/a-causal-models-probability-of-being-true.html" -- interactive
      , "https://wenxin.baidu.com/ernie-vilg" -- interactive
      , "https://ai-muzic.github.io/meloform/" -- low quality (audio embeds)
      , "https://www.webdesignmuseum.org/" -- homepage
      , "https://minerva-demo.github.io/#category=Algebra&index=1" -- interactive
      , "https://yashkant.github.io/housekeep/" -- low quality (video embeds)
      , "https://google-research.github.io/seanet/audiolm/examples/" -- low quality (audio embeds)
      , "https://www.painreprocessingtherapy.com/" -- homepage / low quality (video embeds)
      , "https://namuol.github.io/banal-duck/" -- interactive
      , "https://psych.hanover.edu/JavaTest/CLE/Cognition/Cognition/MentalRotation.html" -- interactive (Java applet!)
      , "https://sites.google.com/view/multi-agent-competition" -- low quality (video embeds)
      , "https://webdiplomacy.net/" -- homepage
      , "https://www.mmlab-ntu.com/project/vtoonify/" -- low quality (video embeds)
      , "https://makeavideo.studio/#facebook" -- low quality (video embeds)
      , "https://www.storylive.com/main.htm" -- interactive
      , "https://sites.google.com/view/projected-gan/" -- low quality (video embeds)
      , "https://diffusion-planning.github.io/"  -- low quality (video embeds)
      , "https://emalmi.kapsi.fi/edit5_code.html" -- low quality (placeholder for future updates)
      , "https://www.robot-learning.uk/dall-e-bot" -- low quality (video embeds)
      , "https://t-naoya.github.io/hdm/" -- low quality (audio embeds)
      , "https://lisiyao21.github.io/projects/AnimeRun" -- low quality (video embeds)
      , "https://vision-locomotion.github.io/" -- low quality (video embeds)
      , "https://ajayj.com/vectorfusion" -- low quality (video embeds)
      , "https://realsinger.github.io/" -- low quality (audio embeds)
      , "https://www.ibd.la/" -- interactive
      , "https://www.complexity-calculator.com/" -- interactive
      , "https://drugs.globalincidentmap.com/" -- interactive (visualization)
      , "https://coveryourtracks.eff.org/" -- interactive (tool)
      , "https://ppubs.uspto.gov/pubwebapp/static/pages/landing.html" -- interactive (database)
      , "https://textsynth.com/" -- interactive (service)
      , "https://dl.acm.org/doi/abs/10.1145/3526113.3545612" -- low quality (video embeds)
      , "https://dataprivacylab.org/dataprivacy/projects/ssnwatch/index.html" -- interactive
      , "https://www.chronophoto.app/game.html" -- interactive (game)
      , "https://people.csail.mit.edu/tzumao/diffvg/" -- low quality (video embeds)
      , "https://discuss.huggingface.co/" -- low quality (layout is very screwed up)
      , "https://blog.metaphysic.ai/the-road-to-realistic-full-body-deepfakes/" -- low quality (video embeds)
      , "https://neeva.com/" -- homepage
      , "https://vectara.com/" -- homepage
      , "https://replicate.com/methexis-inc/img2prompt" -- interactive
      , "https://replicate.com/tommoore515/material_stable_diffusion" -- interactive
      , "https://imagen.research.google/video/" -- low quality (video embeds)
      , "https://juretriglav.si/compressing-global-illumination-with-neural-networks/" -- low quality (video embeds)
      , "https://cs.stanford.edu/~yzzhang/projects/rose/" -- interactive
      , "https://aihabitat.org/" -- low quality (video embeds)
      , "https://magvit.cs.cmu.edu/" -- low quality (video embeds)
      , "https://lexica.art/" -- interactive
      , "https://githubnext.com/projects/ai-for-pull-requests/" -- low quality (video embeds)
      , "https://www.instagram.com/p/Be8vrJAH-8D/" -- low quality (video embeds)
      , "https://overturemaps.org/resources/faq/" -- low quality (collapse/disclosures in the FAQ questions do not work)
      , "https://www.autoregex.xyz/" -- interactive
      , "https://thisimagedoesnotexist.com/" -- interactive (quiz)
      , "https://podcast.ai/" -- low quality (audio embed)
      , "https://www.csm.ai/commonsim-1-generating-3d-worlds-2/" -- low quality (video embeds)
      , "https://wilson1yan.github.io/teco/" -- low quality (video embeds)
      , "https://www.matthewtancik.com/nerf" -- low quality (video embeds)
      , "https://www.reddit.com/r/thisisthewayitwillbe/" -- private
      , "https://valle-demo.github.io/" -- low quality (audio embeds)
      , "https://www.riffusion.com/about" -- low quality (audio embeds)
      , "https://flavioschneider.notion.site/flavioschneider/Audio-Generation-with-Diffusion-c4f29f39048d4f03a23da13078a44cdb" -- low quality (audio embeds)
      , "https://kinyugo.github.io/msanii-demo/" -- low quality (audio embeds)
      , "https://www.tiktok.com/" -- low quality (video embeds)
      , "https://a.tulv.in/algorithms/programming/2021/02/19/finding-mona-lisa-in-the-game-of-life.html" -- low quality (not sure if video embed?)
      , "https://blog.amandaghassaei.com/2022/10/25/digital-marbling/" -- low quality (video embeds)
      , "https://btm.qva.mybluehost.me/building-arbitrary-life-patterns-in-15-gliders/" -- low quality (video embeds)
      , "https://n-e-r-v-o-u-s.com/blog/?p=9225" -- low quality (video embeds)
      , "https://oimo.io/works/life/" -- interactive
      , "https://incredible.pm/" -- interactive
      , "https://www.inkandswitch.com/end-user-programming/" -- low quality (video embeds)
      , "https://hpc.stability.ai/" -- homepage
      , "https://fleg.de/paranoia" -- low-quality (video embeds) + interactive
      , "https://vole.wtf/scunthorpe-sans/" -- interactive
      , "https://collections.library.yale.edu/catalog/2037169" -- interactive
      , "https://dreamix-video-editing.github.io/" -- low quality (video embeds)
      , "https://bost.ocks.org/mike/algorithms/" -- low quality (video embeds) + interactive?
      , "https://pudding.cool/2018/08/wiki-death/" -- low quality (interactive? JS?)
      , "https://mimic-play.github.io/" -- low quality (video embeds)
      , "https://www.alzchem.com/de/" -- homepage
      , "https://pi.ai/talk" -- interactive
      , "https://vecta.io/nano" -- service
      , "https://jukebox.openai.com/" -- interactive
      , "https://www.photoprompts.io/" -- interactive
      , "https://danijar.com/project/dreamerv3/" -- low quality (video embeds)
      , "https://tonyzhaozh.github.io/aloha/" -- low quality (video embeds)
      , "https://maggieappleton.com/transcopyright-dreams#designing-speculative-interfaces" -- low quality (Figma embeds)
      , "https://dl.fbaipublicfiles.com/imagebind/imagebind_video.mp4" -- low quality (video embed - file format?)
      , "https://vcai.mpi-inf.mpg.de/projects/DragGAN/" -- low quality (video embeds)
      , "https://bshall.github.io/knn-vc/" -- low-quality (audio embeds)
      , "https://www.csm.ai/commonsim-1-generating-3d-worlds" -- low quality (video embeds)
      , "https://cohere.com/" -- low quality (fails to archive)
      , "https://tsm-hanlab.mit.edu/" -- low quality (video embeds)
      , "https://muse-model.github.io/" -- low quality (video embeds)
      , "https://accelagent.github.io/" -- low quality (interactive)
      , "http://www.cgg.cs.tsukuba.ac.jp/~endo/projects/UserControllableLT/" -- low quality (video embeds)
      , "https://sites.google.com/view/aistat/home" -- low quality (video embeds)
      , "https://sites.google.com/view/llm-superpositions" -- low quality (contentless)
      , "http://writingclaritycalculator.com/" -- interactive (tool)
      , "https://evolutiongym.github.io/" -- low quality (video embeds)
      , "https://sermanet.github.io/imitate/" -- low quality (video embeds)
      , "https://maayanlab.cloud/Enrichr/" -- interactive
      , "https://www.magicschool.ai/" -- low quality (video embeds)
      , "https://maggieappleton.com/lm-sketchbook#daemons" -- low quality (video embeds)
      , "https://video-ttt.github.io/" -- low quality (video embeds)
      , "https://rl-at-scale.github.io/" -- low quality (video embeds)
      , "https://www.nassiben.com/video-based-crypta" -- low quality (video embeds)
      , "https://leggedrobotics.github.io/rl-perceptiveloco/" -- low quality (video embeds)
      , "https://synthia-dataset.net/" -- low quality (video embeds)
      , "https://www.openstreetmap.org/" -- interactive (tool)
      , "https://styletts2.github.io/" -- low-quality (audio embeds)
      , "https://intxpower.com/" -- interactive (tool)
      , "https://wangyanhui666.github.io/MicroCinema.github.io/" -- low quality (video embeds)
      , "https://bbycroft.net/llm" -- interactive (visualization)
      , "https://gandalf.lakera.ai/" -- interactive (game)
      , "https://google-research.github.io/lingvo-lab/translatotron3/" -- low quality (audio embeds)
      , "https://smerf-3d.github.io/" -- low quality (video embeds)
      , "https://retool.com/pipes" -- low-quality (too much fancy JS; interactive?)
      , "https://nap.nationalacademies.org/read/10668/chapter/1" -- interactive
      , "https://dreamtuner-diffusion.github.io/" -- size/low-quality: final video broke, 199MB
      , "https://hanlab.mit.edu/projects/tsm" -- low-quality (YouTube video embeds)
      , "https://abagames.github.io/crisp-game-lib-11-games/?pakupaku" -- inactive (game) / low-quality (broken in snapshot?)
      , "https://billsworld.neocities.org/" -- low-quality (broken snapshot)
      , "http://timbaumann.info/svd-image-compression-demo/" -- interactive
      , "https://killedbyapixel.github.io/TinyCode/games/CrossMyHeart/" -- interactive game
      , "https://samwho.dev/bloom-filters/" -- interactive
      ]
      -- TODO: add either regexp or full-string match versions so we can archive pages *inside* the subreddit but not the raw subreddit homepage itself
      -- , "https://www.reddit.com/r/politics/" -- homepage
      -- , "https://www.reddit.com/r/keto/" -- homepage
      -- , "https://www.reddit.com/r/AnimeResearch/" -- homepage
      -- , "https://www.reddit.com/r/hangovereffect/" -- homepage
      -- , "old.reddit.com/r/IncreasinglyVerbose" -- service/updated
      -- , "old.reddit.com/r/mlscaling/" -- service/updated
      -- , "old.reddit.com/r/Parahumans/" -- service/updated
      -- , "https://www.reddit.com/r/reinforcementlearning/" -- stable
      -- , "old.reddit.com/r/NavySealCopypasta/" -- updated/service
      -- , "old.reddit.com/r/DataHoarder/" -- news/portal
      -- , "old.reddit.com/r/Piracy/" -- news/portal
      -- , "old.reddit.com/r/PrequelMemes/" -- news/portal
      -- , "old.reddit.com/r/emojipasta/" -- news/portal
      -- , "old.reddit.com/r/explorables/" -- news/portal
      -- , "old.reddit.com/r/MLPtunes/" -- news/portal
      -- , "old.reddit.com/r/TOUHOUMUSIC/" -- news/portal
      -- , "old.reddit.com/r/Vocaloid/" -- news/portal
