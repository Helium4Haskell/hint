-- | A collection of operations on strings and lists.
module StringOps(toLowerCase, isSubsequenceOf, split, trimSpaces)
where



import Data.Char
import Data.List



-- | Converts the string to lower case.
toLowerCase :: String -> String
toLowerCase
  = map toLower


-- | Checks if the first string is a subsequence of the other.
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf key haystack
  = any (key `isPrefixOf`) (tails haystack)


-- | Split a string into substrings separated by key. Empty
--   substrings are discarted.
split :: Eq a => a -> [a] -> [[a]]
split key haystack
  = let (l, r) = break (== key) haystack
        rem    = if null r
                 then []
                 else split key (tail r)
     in if null l
        then rem
        else l : rem


-- | Removes spaces from begin and end of a string.
trimSpaces :: String -> String
trimSpaces
  = reverse. dropWhile (== ' ') . reverse . dropWhile (== ' ')

