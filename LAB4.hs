module PreparationStuff where

import Data.Char
import Data.List
import Data.String

-- prelim1: check if all chars in a string are letters:
checkForLetters :: String -> [Bool]
checkForLetters nx = [isAlpha c | c <- nx]

-- prelim2: get length of string:
getLengthOfString :: String -> Int
getLengthOfString nx = length nx

-- prelim3: shortening a string:
shortenString :: String -> String
shortenString nx = (take (getLengthOfString nx - 1)) nx

-- prelim4: cutting string by one (== last char) if last char is not a letter:
cutStringIfLastCharNotAlpha :: String -> String
cutStringIfLastCharNotAlpha nx
 | and (checkForLetters nx) == False = shortenString nx
 | otherwise = nx

-- prelim5  
countAllStringsInListOfStrings :: [String] -> Int 
countAllStringsInListOfStrings x = length x

-- prelim6
countListOfStringsWithinListOfStrings :: [[String]] -> [Int]
countListOfStringsWithinListOfStrings nx = [countAllStringsInListOfStrings n | n <- nx]

-- prelim7
extractSingleString :: [String] -> [String]
extractSingleString nx = take 1 nx

-----------------------------------------------------------------------------------------------------------------------

-- setp 1: set all to lowercase:
setStringToLowerCase :: String -> String
setStringToLowerCase myStr = [toLower n | n <- myStr]

-- step 2: taking the first string of the list of strings above, checking if all chars are letters 
-- ----------> if the last one isn't: cut that word to the penultuimate char!
cutOutPunctFromStringList :: [String] -> [String]
cutOutPunctFromStringList nx = [cutStringIfLastCharNotAlpha n | n <- nx]

-- step 3: turn string into list of strings (= words from that string) + sort these words alphabetically + cut out any punctuation from every word
sortedListOfAmendedWordsFromString :: String -> [String]
sortedListOfAmendedWordsFromString x = cutOutPunctFromStringList (sort (words (setStringToLowerCase x)))

-- step 4: grouping all sorted + cut words into categories:
groupingAll :: String -> [[String]]
groupingAll nx = group (sortedListOfAmendedWordsFromString nx)

-- step 5:
allCountedWords :: String -> [Int]
allCountedWords nx = countListOfStringsWithinListOfStrings (groupingAll nx)

-- step 6: shortening the list of list of Strings from step 4 to have only one (ie: the first) of each String
takeSingleString :: [[String]] -> [[String]]
takeSingleString nx = [extractSingleString n | n <- nx]

-- step 7: intermediate step: combining step 4 and step 6:
bigCombo :: String -> [[String]]
bigCombo nx = takeSingleString (groupingAll nx)

-- step 8: next intermiediate step: extracting every element from [[String]] into a [String]:
nextBigCombo :: [[String]] -> [String]
nextBigCombo nx = concat nx

-- step 9: final stage: from String to [[String]] to [String]:
finalStage :: String -> [String]
finalStage nx = nextBigCombo (bigCombo nx)

-- step 10: now: pairing Int list from step 5 with String list from setp 9 (using ZIP):
zippingAll :: String -> [(String, Int)]
zippingAll nx = zip (finalStage nx) (allCountedWords nx)

-- step 11a:
width :: Int
width = 40
formatToc :: (String, Int) -> String
formatToc (s, n) = whiteSpaces ++ s ++ "  " ++ replicate n '*'
 where
  whiteSpaces = replicate (width - length s) ' '

-- step 11b:
toc :: [(String,Int)] -> String 
toc es = unlines [formatToc e | e <- es]

-- step 12:
allDoneAtLast :: String -> String
allDoneAtLast nx = toc (zippingAll nx)