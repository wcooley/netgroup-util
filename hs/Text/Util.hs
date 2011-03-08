{-
 - Text.Util - Utility functions for formatting & working with text
 -}

module Text.Util where

-- Join a list of Strings into into a sep-delimited String
-- Similar to 'unlines' except the separator is user-specified and sep is not
-- appended to the end:
--      unlines xs = (join "\n" xs) ++ "\n"
join :: String -> [String] -> String
join sep [] = ""
join sep xs = foldl1 (\x y -> x ++ sep ++ y) xs

-- Make a String of 'n' copies of 'c'
x :: String -> Int -> String
c `x` n = concat(replicate n c)

-- Wrap in double- and single-quotes
dquot :: String -> String
dquot x = "\"" ++ x ++ "\""
squot :: String -> String
squot x = "'" ++ x ++ "'"

-- Single-char substitute for Strings
subst :: Char -> Char -> String -> String
subst frm to src = map (subst1 frm to) src
    where subst1 a b c = if c /= a then c else b
