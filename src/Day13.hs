{-# LANGUAGE Safe #-}
module Day13 (
  day13part1,
  day13part2,
) where

import GHC.Read ( Read(readListPrec, readPrec) )
import Text.Read ((+++))
import qualified Data.List.Split as Data.List
import Data.List (sort)
import Data.Maybe (fromJust)
import GHC.OldList (elemIndex)

data Pkt a = PktVal a | PktList [Pkt a] deriving(Eq)

instance (Read a) => Read (Pkt a) where
  readPrec = pktList +++ pktVal
    where pktVal = PktVal <$> readPrec
          pktList = PktList <$> readListPrec

instance (Show a) => Show (Pkt a) where
  show (PktVal x) = show x
  show (PktList xs) = show xs

instance (Ord a, Eq a) => Ord (Pkt a) where
  compare (PktVal x) (PktVal y) = compare x y
  compare (PktList xs) (PktList ys) = compare xs ys
  compare (PktList xs) y@(PktVal _) = compare xs [y]
  compare x@(PktVal _) (PktList ys) = compare [x] ys

readPairs :: [String] -> [(Pkt Int, Pkt Int)]
readPairs strs = [(read a, read b) | (a, b) <- onlyTwo <$> Data.List.splitOn [""] strs]
  where onlyTwo [a, b] = (a,b)
        onlyTwo xs = error $ "Expected only 2 pairs. Got: " ++ show xs

day13part1 pairs = sum [i | ((a, b), i) <- readPairs pairs `zip` [1..], a <= b]

signal2 :: Pkt Int
signal2 = PktList [ PktList [ PktVal 2 ]]
signal6 :: Pkt Int
signal6 = PktList [ PktList [ PktVal 6 ]]

day13part2 :: [String] -> Int
day13part2 strs = index2 * index6
  where packets :: [Pkt Int]
        packets = sort ([signal2, signal6] ++ (fmap (\ x -> read x :: (Pkt Int)) . filter (not . null) $ strs))
        index2 = 1 + (fromJust . elemIndex signal2 $ packets)
        index6 = 1 + (fromJust . elemIndex signal6 $ packets)