module Day7 (
  Command (..),
  FileOrDir (..),
  dirsWith,
  day1part1,
  splitCommands,
  readCommands,
  readCommand,
) where
import Data.List.Split (split, keepDelimsL, whenElt, dropInitBlank)
import Data.List (isPrefixOf)
import Lib(onlyTwoWords, submatches)

-- assumptions
--   1. all commands are valid
--   2. commands begin with $
--   3. outputs follows a command until a new command is issued $
--   4. cd commands have no output
--   5. ls commands may have no outputs
-- avalaible commands
-- cd /        : brings you back to top level
-- cd ..       : brings you back one level higher
-- cd <string> : bring you down one level
-- ls          : shows you all files and dir


-- first element is the command, second element is the results

data Command = Ls [FileOrDir] | CdUp | CdDown String deriving (Show, Eq)


data FileOrDir = File Int String | Dir String deriving (Show, Eq)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Observation: A cursory glance at day 7 input is a depth-first search traversal

-- |Returns the sum of all directory with size >=
dirsWith :: a
         -> (a -> Int -> a)    -- ^ the predicate function to include the directory or not
         -> [Command]          -- ^ commands left to execute
         -> a                  -- ^ Total size of all directories accepted by the predicate
                               --   Commands left to execute
dirsWith acc0 p cmds = fst3 (_dirsWith acc0 0 p cmds)


day1part1 :: [String] -> Int
day1part1 = dirsWith 0 accFnc . readCommands
  where accFnc acc cwdSize = acc + or0 cwdSize
        or0 i = if i <= 100000 then i else 0


-- acc + or0 p cwdSize

_dirsWith :: a -- ^ Accumulated sum respecting predicate
          -> Int -- ^ Current size of directory
          -> (a -> Int -> a)
          -> [Command]
          -> (a, Int, [Command])
_dirsWith acc cwdSize p [] = (p acc cwdSize, cwdSize, [])
_dirsWith acc cwdSize p (CdUp : remainings) = (p acc cwdSize, cwdSize, remainings)
_dirsWith acc cwdSize p (CdDown _ : remainings) = _dirsWith newAcc (cwdSize + dirSize) p newRemainings
  where (newAcc, dirSize, newRemainings) = _dirsWith acc 0 p remainings
_dirsWith acc cwdSize p (Ls listing : remainings) = _dirsWith acc (cwdSize + totalSize listing) p remainings


totalSize :: [FileOrDir] -> Int
totalSize = sum . map sizeOf
  where sizeOf (Dir _) = 0
        sizeOf (File sz _) = sz

readCommands :: [String] -> [Command]
readCommands = map readCommand . splitCommands

splitCommands :: [String] -> [[String]]
splitCommands = split (keepDelimsL . dropInitBlank $ whenElt (isPrefixOf "$ "))

readCommand :: [String] -> Command
readCommand ("$ ls":listing) = Ls . map readFileOrDir $ listing
readCommand ["$ cd .."] = CdUp
readCommand [str] = CdDown . head . submatches "\\$ cd ([[:word:]\\.]+)" $ str
readCommand rest = error $ "Not a valid command" ++ show rest

readFileOrDir :: String -> FileOrDir
readFileOrDir str
  | "dir " `isPrefixOf` str = Dir . drop 4 $ str
  | otherwise = File (read size) filename
     where (size, filename) = onlyTwoWords str


-- Part 2
-- Total disk space available 70000000
-- Need at least 30000000