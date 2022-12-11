module Lib
  ( palindrome,
    someFunc,
    onlyTwoWords,
    readTwoWords,
    submatches,
    pChar, pString,
  )
where

import Text.Regex.TDFA
import Text.Read

someFunc :: IO ()
someFunc = putStrLn "someFunc"

palindrome :: String -> Bool
palindrome xs
  | xs == reverse xs = True
  | otherwise = False

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

pChar :: Char -> ReadPrec Char
pChar c = do
  c' <- get
  if c == c' then return c else pfail

pString :: String -> ReadPrec String
pString = traverse pChar
