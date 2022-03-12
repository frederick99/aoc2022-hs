{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day4 where

import Data.List ( foldl', groupBy )
import Data.Char ( isDigit, isHexDigit )

data Passport = Passport
    { birthYear :: Maybe String
    , issueYear :: Maybe String
    , expirationYear :: Maybe String
    , height :: Maybe String
    , hairColor :: Maybe String
    , eyeColor :: Maybe String
    , passportID :: Maybe String
    , countryID :: Maybe String }

emptyPassport = Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkPassport = foldl' (flip addField) emptyPassport

addField kvPair passport = case key of
    "byr" -> passport { birthYear = Just value}
    "iyr" -> passport { issueYear = Just value}
    "eyr" -> passport { expirationYear = Just value}
    "hgt" -> passport { height = Just value}
    "hcl" -> passport { hairColor = Just value}
    "ecl" -> passport { eyeColor = Just value}
    "pid" -> passport { passportID = Just value}
    "cid" -> passport { countryID = Just value}
    where
        (key, value) = (takeWhile (/= ':') kvPair, tail $ dropWhile (/= ':') kvPair)

isValid (Passport
        (Just birthYear)
        (Just issueYear)
        (Just expirationYear)
        (Just height)
        (Just hairColor)
        (Just eyeColor)
        (Just passportID)
        _ -- countryID is optional
        ) = True
isValid _ = False

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = filter (notElem c) . groupBy (\a b -> (a == c) == (b == c))


passports = map (mkPassport.words.unwords) . splitOn "" . lines <$> readFile "src/input/Day4.txt"

partOne = passports >>= print . length . filter isValid
-- Answer: 210

------------------------------------------------------
{- Validation rules:
    - byr (Birth Year) - four digits; at least 1920 and at most 2002.
    - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    - hgt (Height) - a number followed by either cm or in:
        - If cm, the number must be at least 150 and at most 193.
        - If in, the number must be at least 59 and at most 76.
    - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    - pid (Passport ID) - a nine-digit number, including leading zeroes.
    - cid (Country ID) - ignored, missing or not. -}
isMoreValid (Passport
            (Just (checkYear 1920 2002 -> True))
            (Just (checkYear 2010 2020 -> True))
            (Just (checkYear 2020 2030 -> True))
            (Just (checkHeight [(150, 193, "cm"), (59, 76, "in")] -> True))
            (Just (isHexColor -> True))
            (Just (isEyeColor -> True))
            (Just (hasDigits 9 -> True))
            _ -- countryID is optional, still
            ) = True 
isMoreValid _ = False

checkYear from to year = all isDigit year && from <= read year && read year <= to

checkHeight specs = flip any specs . heightMatches
heightMatches height (from, to, unit) = from <= read num && read num <= to && rest == unit
    where (num, rest) = span isDigit height

isHexColor ('#':color@[_,_,_,_,_,_]) = all isHexDigit color
isHexColor _ = False

isEyeColor = (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

hasDigits digits num = all isDigit num && length num == digits


partTwo = passports >>= print . length . filter isMoreValid
-- Answer: 131