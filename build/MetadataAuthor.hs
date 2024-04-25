{-# LANGUAGE OverloadedStrings #-}

{- MetadataAuthor.hs: module for managing 'author' metadata:

- canonicalization of names (rewriting all aliases/abbreviations/usernames to a single canonical name which can be easily linked)
- presenting lists of authors in an inline-collapse

18:55:34 < gwern> I've been thinking about how to make the author link working. since the frontend is just displaying the html from the backend,
                  it's not that hard to link authors in a simple way, but it's more of an issue of a scalable long-term-manageable backend. here's what I'm thinking
                  so far: the author GTX field is defined as space-comma-separated lists of authors (each is either plain…
18:55:34 < gwern> …text or an HTML fragment); first, a 'canonicalization' database of fixed-string rewrites is applied to rewrite abbreviations/incomplete
                  names/nicks/usernames to canonical names, like 'ESYudkowsky' (Twitter) or 'Yudkowsky' or 'E. Yudkowsky' -> 'Eliezer Yudkowsky'; this is applied on
                  annotation creation and writing out GTXes to update for any new rewrites. for compilation, authors are…
18:55:34 < gwern> …rewritten using another set of fixed-string rewrites, from author to HTMLized authors (usually an <a> rewrite); these are compiled as normal and
                  will get all the usual archiving & annotating in theory, so there would be something like `("Eliezer Yudkowsky",a "https://yudkowsky.net")`, but
                  there could be non-links or overrides like `("Foo", id)` as appropriate. for disambiguating authors of…
18:55:34 < gwern> …the same name, they can simply be given an arbitrary suffix in the original metadata by hand and then a rewrite used like `("John Smith-1", a
                  "https://foo.com"), ("John Smith-2", a "https://bar.com")`. as a fallback, default Wikipedia links are used: a WP db is checked for True/False/not
                  listed; if True, authors get rewritten to 'https://en.wikipedia.org/wiki/$AUTHOR', False,…
18:55:34 < gwern> …identity/nothing, and if not listed, the WP API is pinged and if there is a non-disambiguation article, it is assumed to be the right guy and added to
                  the WP db with True, otherwise, False. finally, an author-prioritization (similar to the link-icon or x-of-the-day checks) function runs after
                  every full site sync and spits out unhandled authors with >_n_ entries which should have a link defined…
18:55:34 < gwern> …for them or be added to the author-prioritize blacklist.
19:06:04 < gwern> the linkify step doesn't need to care about whether something is linked already because if it has any html in it, then it
                  presumably doesn't match any rewrites. a downside here is that there's no way to look at the GTX entries themselves and say 'oh, X is not linked?
                  but I know exactly where that ought to go'. but if you inject the links back into the GTX to overwrite the…
19:06:04 < gwern> …original plaintext, that has a lot of problems too like outdated links...
19:07:18 < gwern> authors in popups are not linked
19:07:34 <> Authors in popups... OK, I see
19:08:15 < gwern> (currently, I once in a while, when it seems especially important, I will manually add one to the abstract itself like '[<bio>]' or '[<WP>]', but
                  obviously, not great)
19:08:58 <> gwern: So, you're envisioning the author being a link to... what, directly the author's website?
19:09:16 < gwern> well, whatever. usually wikipedia, I expect. most people have pretty crummy sites if they have any
19:12:50 <> gwern: This seems somewhat less than ideal. I see two obvious drawbacks: first, what if the author has multiple URLs associated with
                             them? For example, Eliezer has his yudkowsky.net, also his twitter, also his GW author page - and that's to say nothing of his bio on
                             MIRI's site, his WP entry, etc. Second: what URL it is useful to show the user in reference to an author, depends on the context: if the
                             context is a tweet, then
19:12:50 <>  the author link should definitely at least include his twitter, if not be directly to it.
19:13:36 <> gwern: It seems much more sensible for the author link to pop up an annotation for the author, which should at the least list all
                             applicable URLs for that author (optionally: with the most contextually relevant one highlighted!); and then it can also have a bit of
                             description of the author
19:13:42 < gwern> well, the URL can of course have its own annotation. (I don't know if it would be necessary to avoid self-linking there)
19:14:47 < gwern> so if I really wanted to dump in all the Eliezer URLs, I would now have somewhere logical to do so: whatever URL is chosen as the
                  author-URL (which I didn't before, there's not really any logical place on Gwern.net right now for 'here's 5 useful urls about Eliezer, but I'm not
                  writing an essay or anything about it')
19:15:47 <> I think it would be sensible for the actual link target URL to itself vary depending on context (but the annotation should always be
                             that of the author, and invariant)
19:16:02 < gwern> and since the author fields can just be HTML, they can be overridden in place. if I really want the author URL for a specific
                  Eliezer tweet to be his ESYudkowsky top-level/user twitter page, I just edit the GTX entry accordingly. it won't go there automatically, but as you
                  say, that can't possibly be done automatically in general perfectly
19:16:26 <> eg. for an Eliezer tweet, his name links to his twitter but pops up the EY annotation; for some other mention of Eliezer (perhaps a
                             Sequence post), his name links to his LW author page (but pops up his same annotation)
19:16:42 < gwern> go to half.gtx where the tweet entry is, manually edit 'Eliezer Yudkowsky' (having been canonicalized) to '<a
                  href="twitter.com/ESYudkowsky">Eliezer Yudkowsky</a>', done
19:17:25 < gwern> (as there is no rewrite for '<a href=...', it doesn't get written to yudkowsky.net by the existing 'Eliezer Yudkowsky'->'yudkowsky.net' rule)
19:18:53 <> So, if what I describe is doable, then def. do it IMO
19:19:17 < gwern> and if I need to keep doing this, well, that's where the homonym comes in. there's actually two 'Eliezer Yudkowsky's', Yudkowsky-1 (yudkowsky.net)
                  and Yudkowsky-2 (twitter.com/ESYudkowsky)... the canonicalizer just changes the rule: 'ESYudkowsky'->'Eliezer Yudkowsky-2'
19:19:48 < gwern> and if you want a LW Yudkowsky, well, that's... 'EliezerYudkowsky'? -> 'Yudkowsky-3'
19:20:33 < gwern> (and if you need a URL, the canonicalizer can be passed the entire new annotation and just execute arbitrary functions of all of the fields, from
                  URL to DOI to abstract)
but I think this satisfies most of the
                  needs and fits into the backend reasonably well. and I can experiment easily with improvements like calling LLMs to do web searches to find the
                  best URL for a specific author with specific annotations as context
20:05:15 < gwern> let's see... the WP db would have to live in metadata/, not /static, because with like 25k annotations, and author lists of up to
                  hundreds, and very little author overlap, there's probably easily 50k unique authors. not something to check in, especially with the churn of like
                  every Arxiv paper these days adding a dozen new authors -_-. and because there are so many,…
20:05:15 < gwern> …populating the initial links will be a bit tricky too. I'll probably start with the link-suggester database: take every author name, & pull out
                  the URLs with that name as the link text; that will often be the right author url. then run the WP non-disambig check and associate as many as
                  possible with a WP entry. then just take the frequency list and go one by one...
21:12:30 < gwern> on further thought, I think there is going to be one front-end change necessitated by author links: Wikipedia links need partials
                  now. part of the point of author links is to make them more first class - there's no way to search or browse by author. but with author-links, you
                  get that for free with backlinks - if 'Eliezer Yudkowsky' is linked to yudkowsky.net in every…
21:12:30 < gwern> …relevant full or partial annotation which has the author 'Eliezer Yudkowsky', that is now a backlink. except, of course, for Wikipedia links which
                  are currently excluded from backlinks/partials as having no particular use. but now there will be a use. so I guess implementation-wise, I have to
                  stop excluding WP from the backlinks system, and start writing out partials for WP links, and…
21:12:30 < gwern> …setting .link-annotated-partial on WP links, and the frontend popups code will need to know to wrap WP popups inside a partial-style frame. (the
                  other alternative I thought of, writing out partials which have a WP transclusion inside them, seems a lot worse. more complicated and probably
                  would look a lot worse than special-casing it would be able to do.)
21:39:25 < gwern> it seems like a good idea to do eventually, this will just be a very motivating use-case. doesn't seem too hard? I mean, suppress
                  the title & author compared to a regular partial, but otherwise, the same. a partial... but with a wikipedia article inside it
21:43:59 < gwern> (the wikipedia article can act exactly as usual. it's just the partial frame-wrapper that carries the links to backlinks etc)


Author: Gwern Branwen
Date: 2024-04-14
When:  Time-stamp: "2024-04-14 15:16:14 gwern"
License: CC-0
-}

module MetadataAuthor where

import Data.List (intersperse, intercalate)
import qualified Data.Map.Strict as M (lookup)
import qualified Data.Text as T (find, pack, splitOn, takeWhile, Text)
import Data.Maybe -- (isJust, isNothing)
import Text.Pandoc (Inline(Link, Span, Space, Str), nullAttr)

import Utils (split, frequency)

import qualified Config.MetadataAuthor as C

-- for link bibliographies / tag pages, better truncate author lists at a reasonable length.
-- (We can make it relatively short because the full author list will be preserved as part of it.)
-- simple string-based author-list truncation, with no attempt to do inline-collapse: take the first 100 characters + whatever is necessary to finish the next author (as defined by the space-comma separation)
authorsTruncateString :: String -> String
authorsTruncateString a = let (before,after) = splitAt 100 a in before ++ (if null after then "" else (head $ split ", " after))

authorCollapseTest :: [(String, [Inline])]
authorCollapseTest = filter (\(i,o) -> authorCollapse i /= o) C.authorCollapseTestCases

authorCollapse :: String -> [Inline]
authorCollapse aut
  | aut `elem` ["", "N/A", "N/\8203A"] = []
  | otherwise =
  let authors = intersperse (Str ", ") $ map (\t -> linkify $ T.pack t) $ split ", " aut
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
