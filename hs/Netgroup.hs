{-
 - Netgroup module
 -}

module Netgroup
( Netgroup(..)
, isFlatNetgroup
, flattenNetgroup
, inNetgroup
) where

import Data.Maybe (fromMaybe)

type NetgroupTriple  = String

data Netgroup = Netgroup { netgroup         :: String
                         , description      :: Maybe String
                         , netgroupTriples  :: [NetgroupTriple]
                         , memberNetgroups  :: [Netgroup]
                         }
                        --deriving (Show, Eq)
                        deriving (Eq)

instance Ord Netgroup where
        compare ng1 ng2 = compare (netgroup ng1) (netgroup ng2)


instance Show Netgroup where
    show ng =    "\nNetgroup { netgroup=" ++ (quot (netgroup ng)) ++ ",\n"
              ++ "    description=\"" ++ (fromMaybe "none" (description ng)) ++ "\",\n"
              ++ "    triples=[" ++ (fmt_join (netgroupTriples ng)) ++ "],\n"
              ++ "    members=[" ++ (unlines (map netgroup (memberNetgroups ng))) ++ "]\n"
              ++ "}\n"
        where   fmt_join [] = ""
                fmt_join xs = foldl1 (\x y -> (quot x) ++ sep
                                           ++ (pad 13 " ") ++ (quot y)) xs
                pad n c = concat(replicate n c)
                quot x = "\"" ++ x ++ "\""
                sep = ",\n"


isFlatNetgroup :: Netgroup -> Bool
isFlatNetgroup ng = ( memberNetgroups ng ) == []

flattenNetgroup :: Netgroup -> Netgroup
flattenNetgroup ng = undefined

inNetgroup :: NetgroupTriple -> Netgroup -> Bool
inNetgroup triple ng
        | triple `elem` (netgroupTriples ng)    = True
        | otherwise = any (inNetgroup triple) (memberNetgroups ng)

hasCycle :: Netgroup -> [Netgroup] -> Bool
hasCycle = undefined
-- hasCycle ng seen
--     | ng `elem` (memberNetgroups ng)    = True
--     | otherwise = any (
