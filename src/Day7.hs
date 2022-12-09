module Day7 (
  Command (..),
  FileOrDir (..),
  dirsWith,
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
dirsWith :: (Int -> Bool)    -- ^ the predicate function to include the directory or not
         -> [Command]        -- ^ commands left to execute
         -> Int              -- ^ Total size of all directories accepted by the predicate
                             --   Commands left to execute
dirsWith p cmds = fst3 (_dirsWith 0 0 p cmds)


_dirsWith :: Int -- ^ Accumulated sum respecting predicate
          -> Int -- ^ Current size of directory
          -> (Int -> Bool)
          -> [Command]
          -> (Int, Int, [Command])
_dirsWith acc cwdSize p [] = (acc + or0 p cwdSize, cwdSize, [])
_dirsWith acc cwdSize p (CdUp : remainings) = (acc + or0 p cwdSize, cwdSize, remainings)
_dirsWith acc cwdSize p (CdDown _ : remainings) = _dirsWith newAcc (cwdSize + dirSize) p newRemainings
  where (newAcc, dirSize, newRemainings) = _dirsWith acc 0 p remainings
_dirsWith acc cwdSize p (Ls listing : remainings) = _dirsWith acc (cwdSize + totalSize listing) p remainings

or0 :: (Int -> Bool) -> Int -> Int
or0 p i = if p i then i else 0

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