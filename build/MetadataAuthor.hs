{-# LANGUAGE OverloadedStrings #-}

{- MetadataAuthor.hs: module for managing 'author' metadata & hyperlinking author names in annotations
Author: Gwern Branwen
Date: 2024-04-14
When:  Time-stamp: "2024-05-18 19:12:16 gwern"
License: CC-0

Authors are useful to hyperlink in annotations, but pose some problems: author names are often ambiguous in both colliding and having many non-canonical versions, are sometimes extremely high frequency & infeasible to link one by one, and there can be a large number of authors (sometimes hundreds or even thousands in some scientific fields).
Inserting an author link in the annotation body is highly unsatisfactory, because it doesn't generalize & is clumsy & confusing (a reader naturally expects the regular author name to be a hyperlink, not an odd editorial insertion in the abstract or summary), so we want to hyperlink author names themselves.

This requires multiple logically-separate phases:

1. the annotation format GTX backend allow the 'author' field to contain HTML, as long as it is still comma-separated; this is permitted by the frontend, so we can hyperlink author names arbitrarily in the original annotation as the first level.

    As simple HTML, we can use `<span>`-level collapsing to tame long lists of authors for the reader. And as simple HTML links, they benefit from all existing HTML processing; for example, the author link's URL may be an annotated URL, thereby creating a popup of its own. And such a link is also a standard backlink.

    So we get 'author bibliographies' for free in the form of backlinks: if author X is defined as URL Y, then the backlinks for URL Y are now a list of every URL with author X in the author list.
2. author names are unambiguously comma-separated, so we can parse author fields into lists of authors, and automatically link each one individually by a string match (using a database of name → URL) & rewrite. The author name is truncated at the '#' character to allow disambiguation.
3. further, we can canonicalize author names, by rewriting the original annotation and updating the GTX
4. the canonicalization & link rewrite can be overridden by the original annotation, and can themselves include black/whitelists as necessary.

So let's take a case like "Eliezer Yudkowsky".
This is sometimes written as "E. Yudkowsky", "Eliezer S. Yudkowsky", "E. S. Yudkowsky", or "esyudkowsky" (Twitter username); the canonicalizer rewrites them all to "Eliezer Yudkowsky".
"Eliezer Yudkowsky" gets an auto-link to "https://yudkowsky.net", his homepage.
However, for an annotation of a LessWrong blog post, eg., it might be manually written inline as `<a href=http://lesswrong.com/user/Eliezer_Yudkowsky/>Eliezer Yudkowsky</a>`, or if it is a tweet, to `<a href=https://twitter.com/esyudkowsky>` etc.
If this is common for an author, it would be possible to define 'disambiguated' author names, using the familiar HTML anchor syntax trick we already use for disambiguating multiple annotations of the same URL: the annotation specifies an author name like "Eliezer Yudkowsky#Twitter", and the author link database specifies `("Eliezer Yudkowsky#Twitter", "https://twitter.com/esyudkowsky")`.
This can also be applied to multiple people of the same name, giving them a mnemonic disambiguation: "John Smith#genetics" vs "John Smith#venture-capital" etc.

The initial set of author links was created by pinging Wikipedia for whether a non-disambiguation article existed for the exact author name.
(This resulted in many false positives, but so it goes.)
Subsequent author links can be prioritized by the usual approach of setting up a blacklist of author names, and then regularly reviewing the 𝑛 top author names by site-wide frequency.
This could further come with some browser automation like searching Wikipedia + Google + Google Scholar profile.
-}

module MetadataAuthor where

import Data.List (intersperse, intercalate)
import qualified Data.Map.Strict as M (lookup)
import qualified Data.Text as T (find, pack, splitOn, takeWhile, Text)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Text.Pandoc (Inline(Link, Span, Space, Str), nullAttr)

import Utils (split, frequency)

import qualified Config.MetadataAuthor as C

-- for link bibliographies / tag pages, better truncate author lists at a reasonable length.
-- (We can make it relatively short because the full author list will be preserved as part of it.)
-- simple string-based author-list truncation, with no attempt to do inline-collapse: take the first 100 characters + whatever is necessary to finish the next author (as defined by the space-comma separation)
authorsTruncateString :: String -> String
authorsTruncateString a = let (before,after) = splitAt 100 a in before ++ (if null after then "" else head $ split ", " after)

authorCollapseTest :: [(String, [Inline])]
authorCollapseTest = filter (\(i,o) -> authorCollapse i /= o) C.authorCollapseTestCases

authorCollapse :: String -> [Inline]
authorCollapse aut
  | aut `elem` ["", "N/A", "N/\8203A"] = []
  | otherwise =
  let authors = intersperse (Str ", ") $ map (linkify . T.pack) $ split ", " aut
      authorSpan = if length authors <= 2 then Span ("", ["author", "cite-author"], []) authors
                                               else if length authors < 8 then
                                                      Span ("", ["author"], []) authors
                                                    else let authorsInitial = take 5 authors  -- at >4, we give up trying to display them all & show just the first 3 by default (so we 'snap back' to default 3 authors max, after allowing a bit of elasticity of up to 4, to avoid the situation where we have an inline-collapse with just 1 author tucked away in it - which is annoying because it means cognitive / visual overhead & effort which is then disappointed to see just 1 author hidden - make it worth the reader's while to bother to uncollapse it!)
                                                             authorsRest = drop 5 authors
                                                         in Span ("", ["author", "collapse"], [])
                                                            [Span ("", ["abstract-collapse"], []) authorsInitial
                                                            , Span ("", ["abstract-collapse-only"], []) [Span ("", ["cite-author-plural"], []) []]
                                                            , Span nullAttr authorsRest]
  in [Space, authorSpan]

-- authorsCanonicalizeT :: T.Text -> T.Text
-- authorsCanonicalizeT = T.intercalate ", " . replaceExact (map (\(a,b) -> (T.pack a, T.pack b)) C.canonicals) . T.splitOn ", "
authorsCanonicalize :: String -> String
authorsCanonicalize = intercalate ", " . map (\a -> fromMaybe a (M.lookup a C.canonicals)) . split ", "

authorsLinkify :: T.Text -> [Inline]
authorsLinkify = intersperse (Str ", ") . map linkify . T.splitOn ", "

linkify :: T.Text -> Inline
linkify ""  = Space
linkify " " = Space
linkify aut -- skip anything which might be HTML:
  | isJust (T.find (== '<') aut) || isJust (T.find (== '>') aut) = Str aut -- TODO: my installation of text-1.2.4.1 doesn't provide `T.elem`, so we use a more convoluted `T.find` call until an upgrade
  | otherwise = case M.lookup aut C.authorLinkDB of
                  Nothing -> Str aut
                  Just u -> let aut' = T.takeWhile (/= '#') aut in -- author disambiguation is done by appending an anchor-style disambig like '#foo'; once we have done the lookup, we no longer need it and delete it for display
                                Link nullAttr [Str aut'] (u, "") -- TODO: authorsInitialize -- must be done inside the link-ification step, so skip for now; do we really want to initialize authors at all?

authorPrioritize :: [T.Text] -> [(Int,T.Text)]
authorPrioritize auts = reverse $ frequency $ map fst $
  filter (\(_,b) -> isNothing b) $ map (\a -> (a, M.lookup a C.authorLinkDB)) $
  filter (`notElem` C.authorLinkBlacklist) auts

{-
> md <- LinkMetadata.readLinkMetadata :: IO LinkMetadataTypes.Metadata
> let mdl = Data.Map.toList md :: [(LinkMetadataTypes.Path, LinkMetadataTypes.MetadataItem)]
> take 200 $ authorPrioritize $ map Data.Text.pack $ concatMap (Utils.split ", ") $ map (\(_,(_,aut,_,_,_,_,_)) -> aut) $ mdl
[(86,"Nicholas G. Martin"),(77,"Sergey Levine"),(67,"Peter M. Visscher"),(63,"Dorret I. Boomsma"),(59,"Ian J. Deary"),(57,"Sarah E. Medland"),(57,"Quoc V. Le"),(56,"Pieter Abbeel"),(53,"Jian Yang"),(52,"Kari Stefansson"),(49,"Robert Plomin"),(49,"Oriol Vinyals"),(47,"Naomi R. Wray"),(45,"Ole A. Andreassen"),(43,"Thomas Werge"),(43,"Grant W. Montgomery"),(43,"Andres Metspalu"),(42,"Benjamin M. Neale"),(41,"David Silver"),(41,"Andrew M. McIntosh"),(40,"Stephan Ripke"),(40,"Kenneth S. Kendler"),(37,"Patrick F. Sullivan"),(37,"George Davey Smith"),(36,"Paul Lichtenstein"),(36,"Luke Zettlemoyer"),(36,"Jordan W. Smoller"),(36,"Caroline Hayward"),(35,"Danielle Posthuma"),(34,"Jaakko Kaprio"),(33,"Yejin Choi"),(33,"Matt McGue"),(33,"Chelsea Finn"),(33,"Alec Radford"),(32,"T\245nu Esko"),(32,"Ruth J. F. Loos"),(32,"Michael Boehnke"),(32,"Mark J. Daly"),(32,"Karen Simonyan"),(32,"Anders D. B\248rglum"),(31,"Nancy L. Pedersen"),(31,"Dario Amodei"),(30,"Yoshua Bengio"),(30,"Unnur Thorsteinsdottir"),(30,"Tanner Greer"),(30,"Gonneke Willemsen"),(30,"Aarno Palotie"),(29,"Veikko Salomaa"),(29,"Matthew C. Keller"),(29,"Mark I. McCarthy"),(29,"Joel Gelernter"),(29,"James F. Wilson"),(29,"Gail Davies"),(29,"David M. Hougaard"),(28,"Patrik K. E. Magnusson"),(28,"Nicolas Heess"),(28,"Lili Milani"),(28,"Koray Kavukcuoglu"),(28,"Jouke-Jan Hottenga"),(28,"Gerome Breen"),(28,"David Lubinski"),(27,"William G. Iacono"),(27,"Timothy M. Frayling"),(27,"Nando de Freitas"),(27,"Mike Lewis"),(27,"Michael C. O\8217Donovan"),(27,"Margaret J. Wright"),(27,"Elliot M. Tucker-Drob"),(27,"Arthur R. Jensen"),(26,"Yi Tay"),(26,"Jason Weston"),(26,"Jakob Grove"),(26,"Eric Boerwinkle"),(26,"Andr\233 G. Uitterlinden"),(26,"Andrew C. Heath"),(26,"Albert Hofman"),(25,"Vilmundur Gudnason"),(25,"Srdjan Djurovic"),(25,"Ole Mors"),(25,"Nicholas J. Wareham"),(25,"Manuel Mattheisen"),(25,"Jianfeng Gao"),(25,"Jerome I. Rotter"),(25,"Jascha Sohl-Dickstein"),(25,"Daniel I. Chasman"),(25,"Christian Gieger"),(25,"Abdel Abdellaoui"),(24,"Timothy Lillicrap"),(24,"Stefano Ermon"),(24,"Robert R. Jackson"),(24,"Omer Levy"),(24,"Merete Nordentoft"),(24,"Julian C. Stanley"),(24,"John R. B. Perry"),(24,"Johan G. Eriksson"),(24,"Jared Kaplan"),(24,"Andrew R. Wood"),(23,"Trevor Darrell"),(23,"Tim Salimans"),(23,"Tamara B. Harris"),(23,"Scott D. Gordon"),(23,"Ross Girshick"),(23,"Reedik M\228gi"),(23,"Po-Ru Loh"),(23,"Noah A. Smith"),(23,"Michael J. Owen"),(23,"Markus M. N\246then"),(23,"Loic Yengo"),(23,"Henning Tiemeier"),(23,"Han Zhang"),(23,"Gudmar Thorleifsson"),(23,"Erik Ingelsson"),(23,"Cathryn M. Lewis"),(22,"Yukinori Okada"),(22,"Terho Lehtim\228ki"),(22,"Samuli Ripatti"),(22,"Najaf Amin"),(22,"Mohammad Norouzi"),(22,"Michel G. Nivard"),(22,"Magnus Johannesson"),(22,"K. Paige Harden"),(22,"Jonathan R. I. Coleman"),(22,"John Schulman"),(22,"John P. A. Ioannidis"),(22,"Jie Tang"),(22,"James J. Lee"),(22,"Howard J. Edenberg"),(22,"Harry Campbell"),(22,"Hannaneh Hajishirzi"),(22,"David J. Porteous"),(22,"David A. Hinds"),(22,"Andrew P. Morris"),(22,"Alkes L. Price"),(22,"Alexander Teumer"),(21,"Thore Graepel"),(21,"Shane Legg"),(21,"Sekar Kathiresan"),(21,"Razvan Pascanu"),(21,"Percy Liang"),(21,"Panos Deloukas"),(21,"Noam Shazeer"),(21,"Michael E. Goddard"),(21,"Kari E. North"),(21,"Jonathan Ho"),(21,"Jonas Bybjerg-Grauholm"),(21,"Igor Rudan"),(21,"Igor Mordatch"),(21,"Hreinn Stefansson"),(21,"Harold Snieder"),(21,"Furu Wei"),(21,"Fernando Rivadeneira"),(21,"Cornelia M. van Duijn"),(21,"Colin Raffel"),(21,"Claudia Langenberg"),(21,"Cecilia M. Lindgren"),(21,"Benjamin W. Domingue"),(20,"Sven Cichon"),(20,"Scott Alexander"),(20,"Rol"),(20,"Renato Polimanti"),(20,"Preben Bo Mortensen"),(20,"Paul M. Ridker"),(20,"Neil Houlsby"),(20,"Marcus Hutter"),(20,"Marcella Rietschel"),(20,"Karen L. Mohlke"),(20,"Joulin"),(20,"Jeff Clune"),(20,"George M. Church"),(20,"David Cesarini"),(20,"Bruce M. Psaty"),(20,"Arm"),(20,"Anonymous"),(20,"Anima Anandkumar"),(20,"Anders M. Dale"),(19,"Wei Zhao"),(19,"Tim D. Spector"),(19,"Terrie E. Moffitt"),(19,"Sarah E. Harris"),(19,"Sam McCandlish"),(19,"Ruslan Salakhutdinov"),(19,"Peter Kraft"),(19,"Peter K. Joshi"),(19,"Nicholas J. Timpson"),(19,"Markus Perola"),(19,"Markku Laakso"),(19,"Kathleen Mullan Harris"),(19,"Joel Z. Leibo"),(19,"Ingrid Melle"),(19,"Dan Rujescu"),(19,"Barret Zoph"),(19,"Antonio Torralba"),(18,"Yoichiro Kamatani"),(18,"Samuel R. Bowman"),(18,"Orhan Firat"),(18,"OpenAI"),(18,"Lenore J. Launer"),(18,"Kristian Hveem")]
-}

-- TODO: a function to open up Google+WP searches for the top 5 candidates after every full sync?
