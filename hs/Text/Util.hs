{-
 - Text.Util - Utility functions for formatting & working with text
 -}

module Text.Util where

-- Make a String of 'n' copies of 'c'
x :: String -> Int -> String
c `x` n = concat(replicate n c)

-- Wrap in double- and single-quotes
dquot :: String -> String
dquot x = "\"" ++ x ++ "\""
squot :: String -> String
squot x = "'" ++ x ++ "'"
