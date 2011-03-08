{-
 - Netgroup module
 -}

module Netgroup
( Netgroup(..)
, isFlatNetgroup
, flattenNetgroup
--, inNetgroup
) where

import Data.Maybe (fromMaybe)
import Text.Util

type NetgroupTriple  = String

type NetgroupForest = [Netgroup]

data Netgroup = Netgroup { netgroup         :: String
                         , description      :: Maybe String
                         , netgroupTriples  :: [NetgroupTriple]
                         , memberNetgroups  :: [String]
                         }
                        deriving (Eq)

instance Ord Netgroup where
        compare ng1 ng2 = compare (netgroup ng1) (netgroup ng2)


instance Show Netgroup where
    show ng = "\n" ++ join "\n" [
              "Netgroup { netgroup=" ++ (dquot (netgroup ng))
            , "    description=\"" ++ dquot (fromMaybe "" (description ng))
            , "    triples=[" ++ (join sep (map dquot
                                                (netgroupTriples ng))) ++ "]"
            , "    members=[" ++ (join sep (map dquot
                                                (memberNetgroups ng))) ++ "]"
            , "}\n"
            ]
            where  sep = ",\n" ++ (" " `x` 13)


isFlatNetgroup :: Netgroup -> Bool
isFlatNetgroup ng = ( memberNetgroups ng ) == []

flattenNetgroup :: Netgroup -> Netgroup
flattenNetgroup ng = undefined

--inNetgroup :: NetgroupTriple -> Netgroup -> NetgroupForest -> Bool
{-
inNetgroup triple ng ngf
        | triple `elem` (netgroupTriples ng) = True
        | otherwise = any (inNetgroup triple) (memberNetgroups ng)
-}


hasCycle :: Netgroup -> [Netgroup] -> Bool
hasCycle = undefined
-- hasCycle ng seen
--     | ng `elem` (memberNetgroups ng)    = True
--     | otherwise = any (
