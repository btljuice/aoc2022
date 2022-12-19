module Lib
  (
    flatten,
    isInbound,
    minmax,
    onlyTwo,
    onlyTwoWords,
    pChar, pString,
    readTwoWords,
    replace,
    showCharArray,
    sliding,
    submatches,
    submatches1,
    submatches3,
    trim,
    shortestPath,
  )
where

import Text.Regex.TDFA
import Text.Read
import Data.List (find)
import qualified Data.Foldable

import Data.Array
import Data.List.Extra (dropWhileEnd)
import qualified Data.List.Split

----- List section ----

-- Naive implementation. should do this in one traversal
minmax :: (Ord a, Foldable t) => t a -> (a, a)
minmax fa = (minimum fa, maximum fa)

-- Sliding
sliding :: Int -> [a] -> [[a]]
sliding n xs
  | length xs < n = []
  | otherwise = take n xs : sliding n (drop 1 xs)

-- Slow inefficient list object replacement
replace :: [(Int, a)] -> [a] -> [a]
replace newElems = zipWith replaceElem [0..]
  where replaceElem i e = maybe e snd $ find (\ (i', _) -> i == i') newElems


-- Flattens a List Maybe for example
flatten :: Foldable t => [t a] -> [a]
flatten = Data.Foldable.concatMap Data.Foldable.toList

-- To tuple
onlyTwo :: (Show a) => [a] -> (a, a)
onlyTwo [x, y] = (x, y)
onlyTwo l = error $ "List must be of size 2 : " ++ show l

-------- Algorithm --------

shortestPath :: Eq a => [(a, a)] -> a -> a -> [a]
shortestPath edges start end = reverse . impl $ [[start]]
  where impl [] = []
        impl (path:others)
           | head path == end = path
           | otherwise =
                let nexts = [to:path | (from, to) <- edges, from == head path && notElem to path]
                in  impl (others ++ nexts)

----- parse strings section ---

trim :: String -> String
trim = dropWhile (==' ') . dropWhileEnd (==' ')


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

submatches1 :: String -> String -> String
submatches1 rgx str = case submatches rgx str of
  [m] -> m
  [] -> error $ "Can't match string = '" ++ str ++ "' to regex = '" ++ rgx
  ms -> error $ "Wrong number of matches = '" ++ show ms ++ "' in string = '" ++ str ++ "' to regex = '" ++ rgx

submatches3 :: String -> String -> (String, String, String)
submatches3 rgx str = case submatches rgx str of
  [m1, m2, m3] -> (m1, m2, m3)
  [] -> error $ "Can't match string = '" ++ str ++ "' to regex = '" ++ rgx
  ms -> error $ "Wrong number of matches = '" ++ show ms ++ "' in string = '" ++ str ++ "' to regex = '" ++ rgx


----- ReadPrec helpers when defining custom Read

pChar :: Char -> ReadPrec Char
pChar c = do
  c' <- get
  if c == c' then return c else pfail

pString :: String -> ReadPrec String
pString = traverse pChar

--- Array section

showCharArray :: (a -> Char) -> Array (Int, Int) a -> [String]
showCharArray toChar arr = Data.List.Split.chunksOf width . fmap toChar . Data.Array.elems $ arr
  where ((i0, _), (i1, _)) = Data.Array.bounds arr
        width = i1 - i0 + 1 -- Inclusive range


isInbound :: (Ord a, Ord b) => Array (a, b) c -> (a, b) -> Bool
isInbound hm (i, j) = i0 <= i && i <= i1 && j0 <= j && j <= j1
  where ((i0, j0), (i1, j1)) = Data.Array.bounds hm
