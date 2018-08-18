import Data.Char
import System.Console.Readline

-- Split a grouping of words into word pairs
pairs :: [[Char]] -> [[[Char]]]
pairs [final] = []
pairs     all = [[all !! 0, all !! 1]] ++ (pairs $ tail all)

-- Does a word pair share any letters?
shares :: [[Char]] -> Bool
shares [[], b] = False
shares  [a, b] = first || shares [(tail a), b]
   where first = elem (a !! 0) b

-- All word pairs that do share letters
sharing :: [[[Char]]] -> [[[Char]]]
sharing list = filter shares list

-- Just lowercases things rn tbh
sanitize :: [Char] -> [Char]
sanitize string = map toLower $ filter (/= ',') string

-- Do everything
parse :: [Char] -> [[[Char]]]
parse = pairs . words . sanitize

main = do
  maybeLine <- readline "> "
  case maybeLine of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just ""     -> main
    Just text   -> do
      addHistory text
      print $ sharing $ parse text
      main

