{-
 - Netgroup module
 -}

module Netgroup
{-( Netgroup(..)
, isFlatNetgroup
, flattenNetgroup
, netgroupEdges
, netgroupEdgesByMember
, parseNetgroupTriple
--, inNetgroup
)-} where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.String.Utils
import Data.Tuple.Utils
import Text.Util

type NetgroupTriple = (String,String,String)

type NetgroupForest = [Netgroup]

data Netgroup = Netgroup { netgroup         :: String
                         , netgroupTriples  :: [NetgroupTriple]
                         , memberNetgroups  :: [String]
                         }
                        deriving (Eq)

instance Ord Netgroup where
        compare ng1 ng2 = compare (netgroup ng1) (netgroup ng2)


instance Show Netgroup where
    show ng = "\n" ++ join "\n" [
              "Netgroup { netgroup=" ++ (dquot (netgroup ng))
            , "    triples=[" ++ (join sep (map show
                                                (netgroupTriples ng))) ++ "]"
            , "    members=[" ++ (join sep (map dquot
                                                (memberNetgroups ng))) ++ "]"
            , "}"
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

-- Netgroup triples can have either "-" or the empty string as a placeholder
-- (with different meanings)
real_member :: String -> Bool
real_member mem = mem /= "" && mem /= "-"

filter_user :: [NetgroupTriple] -> [NetgroupTriple]
filter_user = filter ( \(_,u,_) -> real_member u)

filter_host :: [NetgroupTriple] -> [NetgroupTriple]
filter_host = filter ( \(h,_,_) -> real_member h)

-- Format a GraphViz edge
gvedge n0 n1 = " " `x` 4 ++ (dquot n0) ++ " -> " ++ (dquot n1) ++ ";"

-- Users in the immediate netgroup, not recursing into member netgroups
usersImmedInNetgroup :: Netgroup -> [String]
usersImmedInNetgroup ng = map snd3 $ filter_user (netgroupTriples ng)

-- Hosts likewise
hostsImmedInNetgroup :: Netgroup -> [String]
hostsImmedInNetgroup ng = map fst3 $ filter_host (netgroupTriples ng)

-- Create a GraphViz edge from an individual Netgroup
netgroupEdgesByMember :: Netgroup -> [String]
netgroupEdgesByMember ng = [ gvedge (netgroup ng) ms
                            | ms <- memberNetgroups ng ]

netgroupEdgesByHost :: Netgroup -> [String]
netgroupEdgesByHost ng = [ gvedge (netgroup ng) hs
                            | hs <- hostsImmedInNetgroup ng ]

netgroupEdgesByUser :: Netgroup -> [String]
netgroupEdgesByUser ng = [ gvedge (netgroup ng) us
                            | us <- usersImmedInNetgroup ng ]

-- Create big list of GraphViz edges from a list of Netgroups
netgroupEdges :: [Netgroup] -> [String]
netgroupEdges ngs = concat $ ([ netgroupEdgesByMember
                                , netgroupEdgesByHost
                                , netgroupEdgesByUser]
                            <*> ngs)

-- Parse String triple into (String,String,String)
parseNetgroupTriple :: String -> (String,String,String)
parseNetgroupTriple tpl = (\(x:y:z:[]) -> (x,y,z)) $ split "," $ unparen tpl
        where unparen str = if ( startswith "(" str ) && ( endswith ")" str )
                            then init $ tail str
                            else error "malformed triple: " ++ (squot str)
