module QuoteOfTheDay where

-- A simple 'quote of the day' database for storing text snippets which can be transcluded.
--
-- 'Quote of the day' is an old website feature where, for visitors' edification or amusement, a random quote from a list of quotes would be display, often in the website footer or homepage.
-- An example is <https://en.wikiquote.org/wiki/Wikiquote:Quote_of_the_day> which is transcluded in the middle of <https://en.wikiquote.org/wiki/Main_Page>.
-- A common source of quotes used to be <https://en.wikipedia.org/wiki/Fortune_(Unix)>; see also <https://www.lesswrong.com/tag/rationality-quotes>.
--
-- A QOTD database is a `[Quote]` Haskell file, read with read/show. A `Quote` is a 3-tuple `(String, String, Bool)`: HTML "quote", HTML "attribution [or other commentary/metadata]", and whether it has been "used yet" (not entirely necessary, since one could sample randomly, but tracked to minimize reuse of quotes).' When all quotes have been used and are `True`, they get reset to `False` and the cycle begins again (with, presumably, new quotes added since the last time).
-- Quotes & attributions do not contain double-quote delimiters or other HTML wrapping; that will be added when they are formatted as Gwern.net-style 'epigraphs' to be written.
--
-- The QOTD db is used by read in the QOTDB, selecting the first unused quote, marking it used & writing out the updated db, and then writing out the quote to a particular file in a HTML format; that file is used by downstream users such as Hakyll websites which template or transclude it.
-- With the use of `transclude.js`, this can be as simple as:
--
-- `<div class="qotd"><a class="include" href="/metadata/today-quote.html">Quote Of The Day</a></div>`

import Control.Monad (when)
import qualified Data.Set as S (delete, empty, filter, fromList, toList, insert, map)

type QOTDB = [Quote]
type Quote = (String, String, Bool)

quoteDBPath, quotePath :: String
quoteDBPath = "metadata/quotes.hs"
quotePath   = "metadata/today-quote.html"

readQuoteDB :: IO QOTDB
readQuoteDB = fmap read $ readFile quoteDBPath

writeQuoteDB :: QOTDB -> IO ()
writeQuoteDB = writeFile quoteDBPath . show

writeQuote :: Quote -> IO ()
writeQuote (quote,attribution,_) = writeFile quotePath quoted
  where quoted = "<div class=\"epigraph\">\n<blockquote><p>\"" ++ quote ++ "</p>\n<p>" ++ attribution ++ "</p></blockquote>\n</div>"

qnegate :: Quote -> Quote
qnegate (a,b,s) = (a,b,not s)

writeQotD :: IO ()
writeQotD = do db <- fmap S.fromList readQuoteDB
               when (null db) $ error "Fatal error: quote-of-the-day database is empty?"

               -- get set of usable quotes, and if there are none, reset the entire set and use that:
               let dbUnused = S.filter (\(_,_,status) -> not status) db
               let dbReset = if dbUnused /= S.empty then db else S.map qnegate db
               let db' = S.filter (\(_,_,status) -> not status) dbReset

               let qotd = head $ S.toList db'
               writeQuote qotd

               let db'' = S.insert (qnegate qotd) $ S.delete qotd db' -- update the now-used quote
               writeQuoteDB $ S.toList db''
