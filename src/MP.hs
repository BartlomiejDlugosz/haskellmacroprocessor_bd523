module MP (separators, lookUp, splitText, combine, getKeywordDefs, expand) where

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

{-|
This function will look up a key in a list of key-value pairs,
returning all the values that match with that key.

> lookUp "A" [("A", 8), ("B", 9), ("C", 5), ("A", 7)] == [8, 7]
-}
lookUp :: String -> [(String, a)] -> [a]
lookUp s arr = [y | (x,y) <- arr, x == s]

{-|
This function will break up a string with some given separator
characters, returning both the list of separators found between
each "word" and the words themselves.
-}
getNextWord :: [Char] -> String -> (String, String)
getNextWord _ "" = ("", "")
getNextWord sep (x:xs) 
        | x `elem` sep = ("", xs)
        | otherwise = (x : wrd, rem)
        where
              (wrd, rem) = getNextWord sep xs

splitIntoWords :: [Char] -> String -> [String]
splitIntoWords _ "" = [""]
splitIntoWords sep str
       | null([x | x <- str, x `elem` sep]) = [str] -- If there are no seperators left, then just return the string itself
       | otherwise = wrd : splitIntoWords sep rem
       where
              (wrd, rem) = getNextWord sep str

splitText :: [Char] -- ^ the separators to split on
          -> String -- ^ the string to split
          -> ([Char], [String])
splitText sep x = (filter (`elem` sep) x, splitIntoWords sep x)

{-|
This function interleaves the characters from the first argument
list with the strings in the second argument. The second list must
be non-empty.
-}
combine :: [Char] -> [String] -> [String]
combine (x:xs) (y:ys) = y : [x] : combine xs ys
combine _ y = y

{-|
This function takes a list of lines and splits each line to
extract a list of keyword-definition pairs.

> getKeywordDefs ["$x Define x", "$y 55"] == [("$x", "Define x"), ("$y", "55")]
-}
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (x:xs) = let (fst, lst) = getNextWord " " x in (fst, lst) : getKeywordDefs xs

{-|
This function takes the contents of two files, one containing
a template and the other the definitions for each keyword
found within the template. It extracts the keyword-definition
information from the info file and uses it to expand the occurrences
of these keywords in the template file, producing new file contents
as a result.

> expand "The capital $1 is $2" "$1 Peru\n$2 Lima." == "The capital of Peru is Lima"
-}
expand :: FileContents -- ^ the template file contents
       -> FileContents -- ^ the info file contents
       -> FileContents
expand temp info = concat (combine f (map (`replaceWord` keys) s))
       where
              (f, s) = splitText separators temp
              keys = getKeywordDefs (snd (splitText "\n" info))

-- You may wish to uncomment and implement this helper function
-- when implementing expand
replaceWord :: String -> KeywordDefs -> String
replaceWord "" _ = ""
replaceWord wrd keys
       | head wrd == '$' = head(lookUp wrd keys)
       | otherwise = wrd