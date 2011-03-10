{-
 - LDAP.Netgroup module
 -}

module LDAP.Netgroup where

import qualified Data.Map as Map
import Data.Maybe
import LDAP
import LDAP.Util
import Netgroup
import Text.Util

-- Local data

basedn = Just "dc=pdx,dc=edu"

netgroup_attrs = LDAPAttrList [ "cn"
				, "nisNetgroupTriple"
				, "memberNisNetgroup"
				]

netgroup_filter = (Just "(&(objectclass=nisNetgroup)(cn=*))")

search_ldap_netgroup lobj =
                        ldapSearch lobj
                        basedn
                        LdapScopeSubtree
                        netgroup_filter
                        netgroup_attrs
			False


searchNetgroupsInLDAP :: IO [LDAPEntry]
searchNetgroupsInLDAP = do
                    ldapAnonSetup >>= search_ldap_netgroup

netgroupFromLDAP :: LDAPEntry -> Netgroup
netgroupFromLDAP entry =
            Netgroup {
                    netgroup        = netgroup_name entry,
                    netgroupTriples = netgroup_triples entry,
                    memberNetgroups = netgroup_members entry
                 }

-- Attribute extractors to go along with Netgroup records
-- "cn" can be multivalued but should always exist
netgroup_name      = ldapAttr1Yeah "cn"
netgroup_triples = (map parseNetgroupTriple) . ldapAttrYeah "nisNetgroupTriple"
netgroup_members = ldapAttrYeah "memberNisNetgroup"

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

build_netgroups_from_ldap :: IO [Netgroup]
build_netgroups_from_ldap =
    do  m <- ldapNetgroupMap
        return $ map netgroupFromLDAP (Map.elems m)

netgroupGraphIO :: IO String
netgroupGraphIO =
    do  ngs <- build_netgroups_from_ldap
        return $ netgroupGraph ngs

genGraph :: IO ()
genGraph =
    do  netgroupGraphIO >>= putStr
