{-
 - LDAP.Netgroup module
 -}

module LDAP.Netgroup where

import qualified Data.Map as Map
import Data.Maybe
import LDAP hiding (description)
import LDAP.Util
import Netgroup
import Text.Util

-- Local data

basedn = Just "dc=pdx,dc=edu"

netgroup_attrs = LDAPAttrList [ "cn"
				, "description"
				, "nisNetgroupTriple"
				, "memberNisNetgroup"
				]

search_ldap_netgroup lobj =
                        ldapSearch lobj
                        basedn
                        LdapScopeSubtree
                        (Just "(&(objectclass=nisNetgroup)(cn=*))")
                        netgroup_attrs
			False


searchNetgroupsInLDAP :: IO [LDAPEntry]
searchNetgroupsInLDAP = do
                    ldapAnonSetup >>= search_ldap_netgroup

-- netgroupFromLDAP :: IO (Map.Map String LDAPEntry) -> IO LDAPEntry -> IO Netgroup
-- netgroupFromLDAP lngmap entry =
netgroupFromLDAP entry =
            Netgroup {
                    netgroup        = netgroup_name entry,
                    description     = netgroup_description entry,
                    netgroupTriples = netgroup_triples entry,
                    memberNetgroups = netgroup_members entry
                 }

-- Attribute extractors to go along with Netgroup records
-- "cn" can be multivalued but should always exist
netgroup_name      = ldapAttr1Yeah "cn"

-- "description" might be multivalued; not sure; seems to be missing from my
-- installed schema??
netgroup_description = ldapAttr1 "description"
netgroup_triples = ldapAttrYeah "nisNetgroupTriple"
netgroup_members = ldapAttrYeah "memberNisNetgroup"


-- netgroup_members entry lngmap = namesToNetgroups (ldapAttrYeah "memberNisNetgroup" entry) lngmap

-- Given a list of netgroup names, produce a list of Netgroups. Note we also
-- filter for Nothing, so the resulting list may be shorter
{-
namesToNetgroups :: [String] -> IO (Map.Map String LDAPEntry) -> IO [Netgroup]
namesToNetgroups names lngmap =
    sequence [ do { x <- netgroupFromMap n lngmap; return x}  | n <- names ]
-}


-- Produce a Netgroup from an ldapNetgroupMap
-- Note that we must already be certain the entry exists in the
-- ldapNetgroupMap, otherwise we'll get an error
{-
netgroupFromMap :: String -> IO (Map.Map String LDAPEntry) -> IO Netgroup
netgroupFromMap ngname lngmap = ng
                where
                    ldapnetgroup = lookupLDAPNetgroupYeah ngname lngmap
                    ng = netgroupFromLDAP lngmap ldapnetgroup
-}


-- Look up an ldapNetgroup by name in the ldapNetgroup map
lookupLDAPNetgroup :: String -> IO (Map.Map String LDAPEntry)
                            -> IO (Maybe LDAPEntry)
lookupLDAPNetgroup ngname lngmap =
                do  m <- lngmap
                    return $ Map.lookup ngname m

lookupLDAPNetgroupYeah :: String -> IO (Map.Map String LDAPEntry)
                                 -> IO LDAPEntry
lookupLDAPNetgroupYeah ngname lngmap =
                do  e <- lookupLDAPNetgroup ngname lngmap
                    return $ fromJust e

ldapNetgroupMapFromLDAP :: IO [LDAPEntry] -> IO (Map.Map String LDAPEntry)
ldapNetgroupMapFromLDAP entries =
        do  es <- entries
            return $ Map.fromList [ (netgroup_name lng, lng) | lng <- es ]

ldapNetgroupMap :: IO (Map.Map String LDAPEntry)
ldapNetgroupMap = ldapNetgroupMapFromLDAP searchNetgroupsInLDAP

build_netgroups_from_ldap =
    do  m <- ldapNetgroupMap
        return $ map netgroupFromLDAP (Map.elems m)

netgroupEdgesByMember :: Netgroup -> [String]
netgroupEdgesByMember ng = [ " " `x` 12 ++ ngname
                            ++ " -> " ++ (map (subst '-' '_') m) ++ ";"
                            | m <- members ]
    where   ngname = map (subst '-' '_') $ netgroup ng
            members = memberNetgroups ng

subst x y z = if z /= x then z else y

netgroupEdges :: [Netgroup] -> [String]
netgroupEdges ngs = concat $ map netgroupEdgesByMember ngs

netgroupGraph =
    do  ngs <- build_netgroups_from_ldap
        return (unlines $ netgroupEdges ngs)

genGraph =
    do  putStrLn "digraph netgroups {"
        netgroupGraph >>= putStr
        putStrLn "}"
{-
build_netgroups_from_ldap = do
    fmap (map netgroupFromLDAP lngmap) entries
    where
        entries = searchNetgroupsInLDAP
        lngmap = fmap ldapNetgroupMapFromLDAP entries
-}
