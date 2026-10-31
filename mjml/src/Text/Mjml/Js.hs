-- | The handful of JavaScript number semantics MJML's output depends on.
--
-- MJML computes widths with JavaScript numbers and prints them through
-- template strings, so class names such as @mj-column-per-33-333333333333336@
-- and widths such as @300px@ are whatever @Number.prototype.toString@,
-- @parseInt@ and @parseFloat@ make of the arithmetic. Reproducing those
-- functions exactly is what lets this implementation emit the same HTML.
module Text.Mjml.Js (
    showNumber,
    parseIntJs,
    parseFloatJs,
    toFixedNumber,
    roundJs,
    isNaNJs,
) where

import Data.Char (isDigit, isSpace)
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (floatToDigits)


-- | @String(x)@ for a JavaScript number.
showNumber :: Double -> Text
showNumber x
    | isNaN x = "NaN"
    | isInfinite x = if x > 0 then "Infinity" else "-Infinity"
    | x == 0 = "0"
    | x < 0 = "-" <> showNumber (negate x)
    | x < 1e21 && x == fromInteger (truncate x) = T.pack (show (truncate x :: Integer))
    | otherwise =
        let (ds, n) = floatToDigits 10 x
            k = length ds
            digits = T.pack (map toDigitChar ds)
         in if
                | k <= n && n <= 21 -> digits <> T.replicate (n - k) "0"
                | 0 < n && n <= 21 -> T.take n digits <> "." <> T.drop n digits
                | -6 < n && n <= 0 -> "0." <> T.replicate (negate n) "0" <> digits
                | otherwise ->
                    let e = n - 1
                        sign = if e < 0 then "-" else "+"
                        mantissa =
                            if k == 1
                                then digits
                                else T.take 1 digits <> "." <> T.drop 1 digits
                     in mantissa <> "e" <> sign <> T.pack (show (abs e))
  where
    toDigitChar d = toEnum (fromEnum '0' + d)


-- | @parseInt(s, 10)@. 'NaN' when no digits lead the string.
parseIntJs :: Text -> Double
parseIntJs input =
    let s = T.dropWhile isSpace input
        (sign, rest) = splitSign s
        ds = T.takeWhile isDigit rest
     in if T.null ds
            then 0 / 0
            else sign * fromInteger (decimalValue ds)


-- | @parseFloat(s)@. 'NaN' when no number leads the string.
parseFloatJs :: Text -> Double
parseFloatJs input =
    let s = T.dropWhile isSpace input
        (sign, rest) = splitSign s
        intPart = T.takeWhile isDigit rest
        afterInt = T.drop (T.length intPart) rest
        (fracPart, afterFrac) = case T.uncons afterInt of
            Just ('.', r) -> let f = T.takeWhile isDigit r in (f, T.drop (T.length f) r)
            _ -> ("", afterInt)
        expPart = case T.uncons afterFrac of
            Just (e, r)
                | e == 'e' || e == 'E' ->
                    let (esign, r') = splitSign r
                        eds = T.takeWhile isDigit r'
                     in if T.null eds then Nothing else Just (round esign * decimalValue eds)
            _ -> Nothing
     in if T.null intPart && T.null fracPart
            then 0 / 0
            else
                let mantissa =
                        decimalValue intPart * 10 ^ T.length fracPart + decimalValue fracPart
                    scale = toInteger (T.length fracPart) - maybe 0 id expPart
                    value =
                        if scale >= 0
                            then mantissa % (10 ^ scale)
                            else (mantissa * 10 ^ negate scale) % 1
                 in sign * fromRational value


-- | The value of a run of decimal digits.
decimalValue :: Text -> Integer
decimalValue = T.foldl' (\acc c -> acc * 10 + toInteger (fromEnum c - fromEnum '0')) 0


splitSign :: Text -> (Double, Text)
splitSign s = case T.uncons s of
    Just ('-', r) -> (-1, r)
    Just ('+', r) -> (1, r)
    _ -> (1, s)


-- | @Number(x.toFixed(digits))@: round to a fixed number of decimals the way
-- JavaScript does (ties go to the larger value), then read the result back as
-- a number.
toFixedNumber :: Int -> Double -> Double
toFixedNumber digits x
    | isNaN x || isInfinite x = x
    | otherwise =
        let scale = 10 ^ digits :: Integer
            exact = toRational x * fromInteger scale
            n = floor (exact + 1 / 2) :: Integer
         in fromRational (n % scale)


-- | @Math.round(x)@: halves round towards positive infinity.
roundJs :: Double -> Double
roundJs x
    | isNaN x || isInfinite x = x
    | otherwise =
        let r = fromInteger (floor x)
         in if x - r >= 0.5 then r + 1 else r


isNaNJs :: Double -> Bool
isNaNJs = isNaN
