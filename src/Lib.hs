module Lib
  ( palindrome,
    someFunc,
    onlyTwoWords,
    submatches,
  )
where

import Text.Regex.TDFA

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

submatches :: String -> String -> [String]
submatches rgx str = sms
  where (_ ,_, _, sms) = str =~ rgx :: (String, String, String, [String])