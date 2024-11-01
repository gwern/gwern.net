{-# LANGUAGE OverloadedStrings #-}
module Inflation (nominalToRealInflationAdjuster, nominalToRealInflationAdjusterHTML, inflationDollarTestSuite) where

-- InflationAdjuster
-- Author: gwern
-- Date: 2019-04-27
-- When:  Time-stamp: "2024-11-01 14:10:16 gwern"
-- License: CC-0
--
-- Experimental Pandoc module for fighting <https://en.wikipedia.org/wiki/Money_illusion> by
-- implementing automatic inflation adjustment of nominal date-stamped dollar or Bitcoin amounts to
-- provide real prices; Bitcoin's exchange rate has moved by multiple orders of magnitude over its
-- early years (rendering nominal amounts deeply unintuitive), and this is particularly critical in
-- any economics or technology discussion where a nominal price from 1950 is 11x the 2019 real
-- price! (Misunderstanding of inflation may be getting worse over time:
-- <https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3469008> )
-- Prior art: minimal. Most nominal amounts are never inflation-adjusted, and where they are, the author
-- usually settles for either replacing the number, using an ad hoc currency unit like '$2012', or
-- verbosely putting it in parentheses, like '$10 ($100 in 2019)'.
-- (My approach may have been inspired by the 2008 Wikipedia template: <https://en.wikipedia.org/wiki/Template:Inflation>.)
--
-- Years/dates are specified in a variant of my interwiki link syntax (see Interwiki.hs); for example: '[$50]($2000)'
-- or '[₿0.5](₿2017-01-01)'.
-- As a backup syntax (eg. for use inside link anchor texts), one can write it as a span
-- like '[$50]{inflation=$2000)' (ie. `<span data-inflation="$2000">$50</span>`).
-- Dollar amounts use year, and Bitcoins use full dates, as the greater
-- temporal resolution is necessary. Inflation rates/exchange rates are specified in Inflation.hs
-- and need to be manually updated every once in a while; if out of date, the last available rate is
-- carried forward for future adjustments.
--
-- Dollar-years may range 1913–∞, although future years may be meaningless.
-- Dollars are inflation-adjusted using the CPI from 1913 to 1958, then the Personal Consumption
-- Expenditures (PCE) Index thereafter, which is recommended by the Federal Reserve and others as
-- more accurately reflecting consumer behavior & welfare than the CPI.
--
-- Bitcoins are exchange-rate-adjusted using a mix of Pizza Day, historical exchange rates, and
-- Poloniex daily dumps, and their dollar-equivalent inflation-adjusted to the current year. Rates
-- are linearly interpolated for missing in-between dates, and carried forwards/backwards when
-- outside of the provided dataset of daily exchange rates.

{- Examples:
Markdown → HTML:

'[$50.50]($1970)'
 →
'<span class="inflation-adjusted" data-year-original="1970" data-amount-original="50.50" data-year-current="2019" data-amount-current="343.83">$50.50<sub>1970</sub><sup>$343.83</sup></span>'

Testbed:

Dollar inflation example:

$ echo '[$50.50]($1970)' | pandoc -w native
[Para [Link ("",[],[]) [Str "$50.50"] ("$1970","")]]

> nominalToRealInflationAdjuster $ Link ("",[],[]) [Str "$50.50"] ("$1970","")
Span ("",["inflation-adjusted"],[("year-original","1970"),("amount-original","50.50"),("year-current","2020"),("amount-current","231.18"),("title","CPI inflation-adjusted US dollar: from nominal $50.50 in 1970 \8594 real $231.18 in 2020")]) [Str "$231.18",Span ("",["subsup"],[]) [Superscript [Str "$50.50"],Subscript [Str "1970"]]]

$ echo '' | pandoc -f native -w html
<span class="inflation-adjusted" data-year-original="1970" data-amount-original="50.50" data-year-current="2020" data-amount-current="231.18" title="CPI inflation-adjusted US dollar: from nominal $50.50 in 1970 → real $231.18 in 2020">$231.18<span class="subsup"><sub>1970</sub><sup>$50.50</sup></span></span>

Bitcoin deflation example:

$ echo '[₿50.50](₿2017-01-1)' | pandoc -w native
[Para [Link ("",[],[]) [Str "\8383\&50.50"] ("\8383\&2017-01-1","")]]

> :set -XOverloadedStrings
> bitcoinAdjuster (Link ("",[],[]) [Str "\8383\&50.50"] ("\8383\&2017-01-01",""))
Span ("",["inflation-adjusted"],[("year-original","2017-01-01"),("amount-original","50.50"),("year-current","2020"),("amount-current","56,617"),("title","Exchange-rate-adjusted currency: \8383\&50.50 in 2017-01-01 \8594 $56,617")]) [Str "$56,617",Span ("",["subsup"],[]) [Superscript [Str "\8383\&50.50"],Subscript [Str "2017"]]]

$ echo 'Span ("",["inflation-adjusted"],[("year-original","2017-01-01"),("amount-original","50.50"),("year-current","2020"),("amount-current","56,617"),("title","Inflation-adjusted currency: from \8383\&50.50 in 2017-01-01 \8594 $56,617 in 2020")]) [Str "\\$56,617",Math InlineMath "_{\\text{2017}}^{\\text{\8383\&50.50}}"]' | pandoc -f native -w html
<span class="inflation-adjusted" data-year-original="2017-01-01" data-amount-original="50.50" data-year-current="2020" data-amount-current="56,617" title="Exchange-rate-adjusted currency: ₿50.50 in 2017-01-01 → $56,617">$56,617<span class="subsup"><sub>2017</sub><sup>₿50.50</sup></span></span>

$ echo '[$100]{inflation=$2024}' | pandoc -w native
[ Para [ Span ( "" , [] , [ ( "inflation" , "$2024" ) ] ) [ Str "$100" ] ] ]
$ echo '[$100]{inflation=$2024}' | pandoc -w html
<p><span data-inflation="$2024">$100</span></p>
-}

import Data.List (isInfixOf)
import Text.Pandoc (nullAttr, Inline(Link, Span, Str, Subscript, Superscript))
import Text.Read (readMaybe)
import qualified Data.Map.Strict as M (findMax, findMin, fromList, lookup, lookupGE, lookupLE, mapWithKey, Map)
import qualified Data.Text as T (head, length, pack, unpack, tail, Text)

import Config.Misc (currentYear)
import Metadata.Format (printDouble)
import Metadata.Date (isDate)
import Utils (inlinesToText, replace, replaceChecked, sed, toHTML, delete)
import Config.Inflation as C

-- ad hoc dollar-only string-munging version of `nominalToRealInflationAdjuster`, which attempts to parse out an amount and adjust it to a specified date.
-- Intended for use with titles of annotations where dates are available.
-- WARNING: must be used *after* `typesetHtmlField`  / `titlecase'`, as they may break the inline HTML <span>s.
nominalToRealInflationAdjusterHTML :: String -> String -> String
nominalToRealInflationAdjusterHTML _ "" = ""
nominalToRealInflationAdjusterHTML "" s = s
nominalToRealInflationAdjusterHTML date s
  | '$' `notElem` s = s -- no possible amount to adjust
  | "<span data-inflation=\"" `isInfixOf` s || "<span class=\"inflation-adjusted\"" `isInfixOf` s = s -- already adjusted
  -- invalid date to adjust to; NOTE: in theory, this is only called in LinkMetadata.hs & every date read from GTX should have passed `isDate` validation already, and the date is always valid; but we double-check anyway (might be interactive input or something)
  | not (isDate date) = error ("nominalToRealInflationAdjusterHTML: passed a malformed date? " ++ date ++ "; s: " ++ s)
  -- All OK; start adjusting:
  | otherwise =
               let year = "$" ++ take 4 date
                   amount = sed "^(.*),$" "\\1" $ -- delete trailing commas which are not part of the number: eg. 'For $1, you can buy it now!'
                            sed "^.*(\\$[1-9][0-9,.]*).*$" "\\1" s
                   inflated = nominalToRealInflationAdjuster $ Link nullAttr [Str (T.pack amount)] (T.pack year, "")
                   inflatedString = Utils.toHTML inflated
               in if amount == inflatedString then s else replaceChecked amount inflatedString s -- NOTE: `replace`/`replaceChecked` requires the arguments to be different to avoid bugs, so we must guarantee that we do not attempt to run an identity transform; and we ensure that the replacement happened.

nominalToRealInflationAdjuster :: Inline -> Inline
nominalToRealInflationAdjuster x@(Span (a, b, [(k, v)]) inlines) = if  k /= "inflation" then x
                                                                     else nominalToRealInflationAdjuster (Link (a,b,[]) inlines (v,""))
nominalToRealInflationAdjuster x@(Link _ _ ("", _)) = error $ "Inflation adjustment (Inflation.hs: nominalToRealInflationAdjuster) failed on malformed link: " ++ show x
nominalToRealInflationAdjuster x@(Link _ _ (ts, _))
  | t == '$'     = dollarAdjuster currentYear x
  | t == '\8383' = bitcoinAdjuster currentYear x --- official Bitcoin Unicode: '₿'/'\8383'; obsoletes THAI BAHT SIGN
  where t = T.head ts
nominalToRealInflationAdjuster x = x

-- hardwired for 2023 results.
inflationDollarTestSuite :: [((T.Text, -- test-case: Amount
                               T.Text), -- test-case: Original Year
                               Inline, -- Expected result
                               Inline)] -- Actual result (which ≠ Expected)
inflationDollarTestSuite = filter (\(_,expect,result) -> expect/=result) $ map (\((t,y), expected) -> ((t,y), expected, dollarAdjuster 2023 (Link ("",[],[]) [Str t] (y, "")))) C.inflationDollarLinkTestCases

-- TODO: refactor dollarAdjuster/bitcoinAdjuster - they do *almost* the same thing, aside from handling year vs dates
dollarAdjuster :: Int -> Inline -> Inline
dollarAdjuster _ l@(Link _ _ ("", _)) = error $ "Inflation.hs: dollarAdjuster: Inflation adjustment failed on malformed link (no old-date specified): " ++ show l
dollarAdjuster currentyear l@(Link _ text (oldYears, _)) =
  -- if the adjustment is <X%, don't bother, it's not misleading enough yet to need adjusting:
  if text' == "" || head text' /= '$' then error $ "Inflation.hs: dollarAdjuster: amount text must begin with a dollar sign: " ++ show l
  else
   if (adjustedDollar / oldDollar) < C.minPercentage || oldDollar == 0 -- (0/0) yields NaN which fails comparison so we would try to inflation-adjust $0
   then Span nullAttr text -- just strip out the inflation annotation & do nothing
   else Span ("", -- no unique identifier available
              ["inflation-adjusted"], -- CSS/HTML class for styling
              -- provide all 4 variables as metadata the <span> tags for possible CSS/JS processing
              [("year-original",oldYear),("amount-original",T.pack oldDollarString),
               ("year-current",T.pack $ show currentyear),("amount-current",T.pack adjustedDollarString),
               ("title", T.pack ("CPI inflation-adjusted US dollar: from nominal $"++oldDollarString'++" in "++T.unpack oldYear++" → real $"++adjustedDollarString++" in "++show currentyear)) ])
        -- [Str ("$" ++ oldDollarString), Subscript [Str oldYear, Superscript [Str ("$"++adjustedDollarString)]]]
        [Str (T.pack $ "$"++adjustedDollarString),  Span ("",["subsup"],[]) [Superscript [Str $ T.pack $ "$" ++ oldDollarString'], Subscript [Str oldYear]]]
    where text' = delete " " $ replace " " "," $ T.unpack $ inlinesToText text
          -- oldYear = '$1970' → '1970'
          oldYear = if T.length oldYears /= 5 || T.head oldYears /= '$' then error (show l) else T.tail oldYears
          -- '$50.50' → '50.50'; '$50.50k' → '50500.0'; '$50.50m' → 5.05e7; '$50.50b' → 5.05e10; '$50.50t' → 5.05e13
          oldDollarString = multiplyByUnits l $ filter (/= '$') text'
          oldDollar = case (readMaybe $ filter (/=',') oldDollarString :: Maybe Double) of { Just d -> d; Nothing -> error ("Inflation: oldDollar: " ++ show l ++ ":" ++ oldDollarString) }
          oldDollarString' = printDouble 2 oldDollar
          adjustedDollar = dollarAdjust currentyear oldDollar (T.unpack oldYear)
          adjustedDollarString = printDouble 2 adjustedDollar
dollarAdjuster _ x = x

multiplyByUnits :: Inline -> String -> String
multiplyByUnits l "" = error $ "Inflation.hs (dollarAdjuster): an empty amount was processed from 'text' variable. Original input: " ++ show l
multiplyByUnits l amount = let (unit, rest) = (last amount, read (init $ filter (/=',') amount) :: Double) in -- eg. '100m' → ('m',"100")
                           if unit `elem` ("0123456789"::String) then amount else show $ case unit of
                                                                              'k' -> rest*1000
                                                                              'm' -> rest*1000000
                                                                              'b' -> rest*1000000000
                                                                              't' -> rest*1000000000000
                                                                              e -> error $ "Inflation.hs (dollarAdjuster:multiplyByUnits): a malformed unit multiplier appeared in 'text' variable. Attempted unit multiplication by '" ++ show e ++ "'; original: " ++ show l

-- dollarAdjust "5.50" "1950" → "59.84"
dollarAdjust :: Int -> Double -> String -> Double
dollarAdjust cy amount year = case (readMaybe year :: Maybe Int) of
                                Just oldYear -> inflationAdjustUSD amount oldYear cy
                                Nothing -> error (show amount ++ " " ++ year)

-- inflationAdjustUSD 1 1950 2019 → 10.88084
-- inflationAdjustUSD 5.50 1950 2019 → 59.84462
inflationAdjustUSD :: Double -> Int -> Int -> Double
inflationAdjustUSD d yOld yCurrent = if yOld>=1913 && yCurrent>=1913 then d * totalFactor else d
  where slice from to xs = take (to - from + 1) (drop from xs)
        percents = slice (yOld-1913) (yCurrent-1913) C.inflationRatesUSD
        rates = map (\r -> 1 + (r/100)) percents
        totalFactor = product rates

bitcoinAdjuster :: Int -> Inline -> Inline
bitcoinAdjuster _ l@(Link _ _ ("", _)) = error $ "Inflation adjustment (bitcoinAdjuster) failed on malformed link: " ++ show l
bitcoinAdjuster currentyear l@(Link _ text (oldDates, _)) =
   Span ("",
            ["inflation-adjusted"],
            [("year-original",oldDate),         ("amount-original",T.pack oldBitcoinString),
             ("year-current",T.pack $ show currentyear), ("amount-current", T.pack adjustedBitcoinString),
             ("title", T.pack ("Exchange-rate-adjusted currency: \8383"++oldBitcoinString++" in "++T.unpack oldDate++" → $"++adjustedBitcoinString)) ])
      [Str (T.pack $ "$"++adjustedBitcoinString),  Span ("",["subsup"],[]) [Superscript text, Subscript [Str (T.pack oldYear)]]]
  where oldDate = if T.length oldDates /= 11 || T.head oldDates /= '\8383' then error (show l) else T.tail oldDates
        oldBitcoinString = filter (/= '\8383') $ T.unpack $ inlinesToText text
        oldBitcoin = case (readMaybe (filter (/=',') oldBitcoinString) :: Maybe Double) of
                       Just ob -> ob
                       Nothing -> error (show l)
        oldYear = take 4 $ T.unpack oldDate -- it takes up too much space to display full dates like '2017-01-01'; readers only really need the year; the exact date is provided in the tooltip
        oldDollar = bitcoinAdjust currentyear oldBitcoin (T.unpack oldDate)
        adjustedDollar = dollarAdjust currentyear oldDollar oldYear
        adjustedBitcoinString = printDouble 2 adjustedDollar

      -- oldYear = if T.length oldYears /= 5 || T.head oldYears /= '$' then error (show l) else T.tail oldYears
      -- oldDollarString = multiplyByUnits l $ filter (/= '$') $ T.unpack $ inlinesToText text -- '$50.50' → '50.50'; '$50.50k' → '50500.0'; '$50.50m' → 5.05e7; '$50.50b' → 5.05e10; '$50.50t' → 5.05e13
      -- oldDollar = case (readMaybe (filter (/=',') oldDollarString) :: Maybe Double) of
      --               Just d -> d
      --               Nothing -> error (show l)
      -- oldDollarString' = show oldDollar
      -- adjustedDollar = dollarAdjust oldDollar (T.unpack oldYear)
      -- adjustedDollarString = show adjustedDollar
bitcoinAdjuster _ x = x

-- convert to historical USD, and then inflation-adjust the then-exchange rate to the present day for a real value
bitcoinAdjust :: Int -> Double -> String -> Double
bitcoinAdjust cy oldBitcoinAmount oldDate = let oldExchangeRate = bitcoinQuery cy oldDate in oldBitcoinAmount * oldExchangeRate

-- Look up USD/₿ daily exchange rate for a given day using a hardwired exchange rate database; due to the extreme volatility of Bitcoin, yearly exchange rates are not precise enough.
-- If the requested date is after the last available date, the last exchange rate is carried forward indefinitely; if the date is inside the database range but not available (due to spotty time-series), linearly interpolate (average) the two nearest rates before & after; if the date is before the first well-known Bitcoin purchase (Pizza Day), carry that backwards indefinitely.
bitcoinQuery :: Int -> String -> Double
bitcoinQuery cy date = case M.lookup date db of
                        Just rate -> rate
                        -- like inflation rates, we carry forward the last available exchange rate
                        Nothing -> let (lastDate,lastRate) = M.findMax db in
                                     let (firstDate,firstRate) = M.findMin db in
                                         if date > lastDate then lastRate else
                                           if date < firstDate then firstRate else
                                             let afterTuple = M.lookupGE date db
                                                 beforeTuple = M.lookupLE date db in
                                               case (afterTuple, beforeTuple) of
                                                 (Nothing, _) -> error $ "Inflation.bitcoinQuery: after-date lookup failed, this should never happen: " ++ show cy ++ show date ++ show afterTuple ++ show beforeTuple
                                                 (_, Nothing) -> error $ "Inflation.bitcoinQuery: before-date lookup failed, this should never happen:" ++ show cy ++ show date ++ show afterTuple ++ show beforeTuple
                                                 (Just (_,after), Just(_,before)) -> (after + before) / 2
  where db = bitcoinUSDExchangeRate cy

-- the exchange rates are, of course, historical: a 2013 USD/Bitcoin exchange rate is for a *2013* dollar, not a current dollar. So we update to a current dollar.
bitcoinUSDExchangeRate :: Int -> M.Map String Double
bitcoinUSDExchangeRate cy = M.mapWithKey (\dt amt -> inflationAdjustUSD amt (read (take 4 dt)::Int) cy) (M.fromList bitcoinUSDExchangeRateHistory)

{- This general approach of automatically inflation-adjusting dollar/₿ amounts to the current nominal (ie. real) amount, rather than default to presenting ever-more-misleading nominal historical amounts, could be applied to many other financial assets.
As far as almost all financial software is concerned, 'a dollar is a dollar', and as long as credits & debits sum to zero, everything is fine; inflation is, somewhat bizarrely, almost completely ignored.
Here are 4 ideas of 'inflation adjustments' to make commonly-cited economic numbers more meaningful:

1. rewrite individual stock returns to be always against a standard stock index, rather than against themselves in nominal terms
2. annotate stock prices on a specific date with their #1 statistic from that date to 'today'
3. rewrite currencies to year-currencies, with 'real' currency amounts simply being reported in the most recent year-currency
4. rewrite year-currencies into the NPV of the next-best alternative investment, which represents the compounding opportunity cost

Stock prices would benefit from being reported in meaningful terms like net real return compared to
an index like the S&P 500 (reinvesting), as opposed to being reported in purely nominal terms: how often do we
really care about the absolute return or %, compared to the return over alternatives like the
default baseline of simple stock indexing? Typically, the question is not 'what is the return from
investing in stock X 10 years ago?' but 'what is its return compared to simply leaving my money in
my standard stock index?'. If X returns 70% but the S&P 500 returned 200%, then including any
numbers like '70%' is actively misleading to the reader: it should actually be '−130%' or something,
to incorporate the enormous opportunity cost of making such a lousy investment like X.
(See <https://marginalrevolution.com/marginalrevolution/2016/10/performance-pay-nobel.html>.)

Another example of the silliness of not thinking about the *use* of numbers: ever notice those stock tickers in
financial websites like WSJ articles, where every mention of a company is followed by today's stock
return ("Amazon (AMZN: 5%) announced Prime Day would be in July")? They're largely clutter: what
does a stock being up 2.5% on the day I happen to read an article tell me, exactly? But what *could*
we make them mean? In news articles, we have two categories of questions in mind:

1. how it started (how did the efficient markets *react*)?

    When people look at stock price movements to interpret whether news is better or worse than
    expected, they are implicitly appealing to the EMH: "the market understands what this means, and
    the stock going up or down tells us what it thinks". So 'tickercruft' is a half-assed implicit
    'event study' (which is only an event if you happen to read it within a few hours of
    publication—if even that). "GoodRx fell −25% when Amazon announced online pharmacy. Wow, that's
    serious!"

    To improve the event study, we make this rigorous: the ticker is meaningful only if it captures
    the *event*. Each use must be time-bracketed: what exact time did the news break & how did the
    stock move in the next ~hour (or possibly day)? Then that movement is cached and displayed
    henceforth. It may not be perfect but it's a lot better than displaying stock movements from
    arbitrarily far in the future when the reader happens to be reading it.
2. How's it been going (since then)?

    When we read news, to generalize event studies, we are interested in the long-term outcome.
    "It's a bold strategy, Cotton. Let's see how it works out for them." So, similar to considering
    the net return for investment purposes, we can show the (net real index adjusted) return since
    publication. The net is a high-variance but unbiased estimator of every news article, and useful
    to know as foreshadowing: imagine reading an old article with the sentence "VISA welcomes its
    exciting new CEO John Johnson (V: −30%)." This is useful context. V being up 0.1% the day you
    read the article, is not.

Tooling-wise, this is easy to support. They can be marked up the same way, eg. '[AMZN](!N
"2019-04-27")' for Amazon/NASDAQ. For easier writing, since stock tickers tend to be unique (there
are not many places other than stock names that strings like "AMZN" or "TSLA" would appear), the
writer's text editor can run a few thousand regexp query-search-and-replaces (there are only so many
stocks) to transform 'AMZN' → '[AMZN](!N "2019-04-27")' to inject the current day automatically.
(This could also be done by the CMS automatically on sight, assuming first-seen = writing-date,
although with Pandoc this has the disadvantage that round-tripping does not preserve the original
Markdown formatting, and the Pandoc conventions can look pretty strange compared to 'normal'
Markdown—at least mine, anyway.)

It would also be useful to make temporal adjustments in regular accounting software, to treat each year of a currency as a foreign currency, with inflation defining the temporal exchange rate. One would have year-currencies like '2019 dollars' vs '2020 dollars' and so on, and compute with respect to a particular target-year like '2024 dollars'. This allows for much more sensible inter-temporal comparisons of expenditures or income, which do not simply reflect (usually unrelated) inflation rate changes and weird expenditure-currency-time-weighted nominal sums, and which would make clearer things like when real income is declining due to lack of raises.
Accounting software like ledger/hledger already mostly support such things through their foreign currencies, but they don't make it necessarily easy to write. Some syntactic sugar would help. Like '$' is always a shortcut to USD, regardless of whether that is a dollar in 1792 or 2024; but it could be defined as simply referring to the USD in the year of that transaction and be equivalent to a year-currency like '2024$'.

We could go further: one should not hold much currency, because it is a bad investment. If you hold a dollar permanently, you are foregoing a better alternative investment (like a stock index, eg. VTSAX), so you suffer from both inflation *and* severe opportunity cost. Often, the opportunity cost is a lot larger, too.
Accounting exists reflect the reality of our decisions & financial health: this is the ultimate goal of accounting which goes beyond just a crude list of account credits/debits or list of assets, which inevitably requires building a simple 'model', with judgments about risks and depreciation and opportunity cost (which is why 'cash flow is a fact, all else is opinion').
But simply listing everything in dollar amounts fails to do this.
When we compare individual stocks to the S&P 500, why stop there? Why not denominate *everything* in S&P 500 shares?
This would better reflect our actual choice ("spend on X" vs "save in VTSAX").

(This is also why accounting systems are never feature-complete, and always keep sprouting new ad hoc features & flags & reports, eventually embodying Greenspun's law with their own, often ad hoc and extremely low-quality, built-in programming language. Accounting is not a simple problem of just tracking some movements of a few kinds of assets like 'dollars' or 'cash' in and out of some 'accounts'. Accounting is as complex as the world, because we always want more informative models of our financial position, and have more domain-specific knowledge to encode, and people will always differ about what they think the causal models are or what the future will be like, and this will cause conflicts over 'accounting'. Accounting is ultimately for *decision-making*: a failure to make this explicit will lead to many confusions & illusions in understanding the purpose of accounting or things like 'GAAP'. (See also <https://gwern.net/research-criticism>.) Accounting systems should never delude themselves about this intrinsic complexity by pretending it doesn't exist and trying to foist it onto the users, but accept that they have to handle it and provide real solutions, like being built with a properly supported language like a Lisp or Python, instead of fighting a rearguard action by adding on features ad hoc as users can ram them through to meet their specific needs.)

Like the year-currencies, this is fine as far as it goes, but it suffers from its own version of 'nominal' vs 'real', by omitting a kind of intrinsic 'deflation' from the opportunity cost of compounding: if we simply do it the default way by defining a daily VTSAX exchange rate and inputting everything in year-currencies, which get converted to final VTSAX numbers for reports, we do take into account the increase in value of a VTSAX share over time, but we do not take into account something fairly important—the *income* from the index.
This would systematically bias reports by when a share was purchased: older shares are more valuable, assuming we buy-and-hold, and especially if we reinvest that.
(VTSAX pays out relatively little as dividends, so the problem isn't so big, but for other alternatives, like bonds, this might be wildly misleading.)

We presumably do track income from all investments properly, and so indirectly the opportunity cost is reflected in the balance sheets; the issue is not that it leads to wrong final numbers, but that it obscures the financial reality.
That of course is true of everything in accounting (sooner or later it all shows up in the assets or cashflow—fail to track depreciation properly and eventually there will be a large expensive expense for replacement/repair; fail to buy insurance or self-insure, and an expensive risk will happen etc), and the point of accounting is to *show* such things beforehand, and let us see the costs & benefits *now*.

So the daily VTSAX share price (mark-to-market) should instead be treated as mark-to-maturity (ie. we plan to do something like "hold it until the year 202X, after which we will sell it during retirement"), and the daily mark reflects the predicted NPV of holding VTSAX for Y years while reinvesting & compounding.
The further back in time the VTSAX share, the more the 'exchange rate' reflects the future income & compounding (because we require several 2024 VTSAX shares to equal the value of 1 VTSAX share we bought in 2000, because that 2000 share *turned into* several 2024 VTSAX shares all on its own).
Then we get correct real opportunity-costs of expenses over time: our decision to spend $10 on Beanie Babies in 2000 correctly reflects the real long-term cost to us of not putting that money into an index, and we can compute out the actual (or predicted) cost at any given date, and better see whether that asset performed well or the expense was justified. (*Did* investing in those Beanie Babies beat the alternative, or did our asset total gradually wither each year VTSAX outperformed and in retrospect the decision got worse and we refused to realize the loss? *Did* that $10 of ice cream as a teen make us happier than $100 in retirement we can't spend on anything we care about?)
This requires a bit more implementation effort, but still is not too hard: the target date can be specified by the user upfront once, and a simple, fair extrapolation of future income/returns used (eg. a simple average of all historical ones). If the user has so much that they can't save tax-free, a fudge factor can be added for taxes (updated after each actual tax filing if the user desires the greater accuracy). As the daily VTSAX share prices are updated however they are normally, the NPV estimate can be updated by the ledger software without any further work from the user. And '$' doesn't need to be further modified, it simply remains a year-currency like the previous idea.
So for the user, this should be scarcely any more difficult than normal accounting software usage: one inputs transactions every day in a normal dollar amount, the stock price is updated by a script or API, and the main user-visible change is defaulting to 'VTSAX_202X' units instead of '2024$' units, unless the user asks for '2024$' reports etc.
-}
