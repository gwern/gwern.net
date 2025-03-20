{-# LANGUAGE OverloadedStrings #-}
module Inflation (nominalToRealInflationAdjuster, nominalToRealInflationAdjusterHTML, inflationDollarTestSuite, isInflationURL, isInflationLink) where

-- InflationAdjuster
-- Author: gwern
-- Date: 2019-04-27
-- When:  Time-stamp: "2025-03-20 09:32:17 gwern"
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
--
-- For discussion of other possible ways to adjust currency amounts or stocks, see <https://gwern.net/subscript#inflation>.

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

import qualified Config.Misc as CM (currentYear)
import Metadata.Format (printDouble)
import Metadata.Date (isDate)
import Utils (inlinesToText, replace, replaceChecked, sed, toHTML, delete, isInflationURL, isInflationLink)
import qualified Config.Inflation as C (inflationRatesUSD, bitcoinUSDExchangeRateHistory, minPercentage, inflationDollarLinkTestCases)

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
  | t == '$'     = dollarAdjuster CM.currentYear x
  | t == '\8383' = bitcoinAdjuster CM.currentYear x --- official Bitcoin Unicode: '₿'/'\8383'; obsoletes THAI BAHT SIGN
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
                       Nothing -> error $ "Inflation.bitcoinAdjuster: 'oldBitcoin' failed at reading 'oldBitcoingString' into a double; 'oldBitcoinString' was: " ++ show oldBitcoinString ++ "; Inline input was: " ++ (show l)
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
bitcoinUSDExchangeRate cy = M.mapWithKey (\dt amt -> inflationAdjustUSD amt (read (take 4 dt)::Int) cy) (M.fromList C.bitcoinUSDExchangeRateHistory)

