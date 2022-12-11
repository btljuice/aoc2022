module Lib
  (
    onlyTwoWords,
    readTwoWords,
    replace,
    submatches,
    pChar, pString,
    flatten,
  )
where

import Text.Regex.TDFA
import Text.Read
import Data.List (find)
import qualified Data.Foldable

----- List section ----
-- Slow inefficient list object replacement
replace :: [(Int, a)] -> [a] -> [a]
replace newElems = zipWith replaceElem [0..]
  where replaceElem i e = maybe e snd $ find (\ (i', _) -> i == i') newElems


-- Flattens a List Maybe for example
flatten :: Foldable t => [t a] -> [a]
flatten = Data.Foldable.concatMap Data.Foldable.toList



----- parse strings section ---

onlyTwoWords :: String -> (String, String)
onlyTwoWords l = case words l of
  [a, b] -> (a, b)
  _ -> error $ "Line as not 2 words :" ++ show l

readTwoWords :: (Read a, Read b) => String -> (a, b)
readTwoWords str = (read a, read b)
  where (a, b) = onlyTwoWords str

submatches :: String -> String -> [String]
submatches rgx str = sms
  where (_ ,_, _, sms) = str =~ rgx :: (String, String, String, [String])


----- ReadPrec helpers when defining custom Read

pChar :: Char -> ReadPrec Char
pChar c = do
  c' <- get
  if c == c' then return c else pfail

pString :: String -> ReadPrec String
pString = traverse pChar
