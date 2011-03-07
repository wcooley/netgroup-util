{-
 - LDAP.Netgroup module
 -}

module LDAP.Netgroup where

import qualified Data.Map as Map
import Data.Maybe
import LDAP hiding (description)
import LDAP.Util
import Netgroup

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
                        (Just "(&(objectclass=nisNetgroup)(cn=b*))")
                        netgroup_attrs
			False
    

searchNetgroupsInLDAP :: IO [LDAPEntry]
searchNetgroupsInLDAP = do
                    ldapAnonSetup >>= search_ldap_netgroup

-- netgroupFromLDAP :: IO (Map.Map String LDAPEntry) -> IO Netgroup
netgroupFromLDAP ngmap entry =
        Netgroup {  netgroup        = netgroup_name entry,
                    description     = netgroup_description entry,
                    netgroupTriples = netgroup_triples entry,
                    memberNetgroups = netgroup_members entry ngmap
                 }

-- Attribute extractors to go along with Netgroup records
-- "cn" can be multivalued but should always exist
netgroup_name      = ldapAttr1Yeah "cn"

-- "description" might be multivalued; not sure; seems to be missing from my
-- installed schema??
netgroup_description = ldapAttr1 "description"
netgroup_triples = ldapAttrYeah "nisNetgroupTriple"

netgroup_members_ entry ngmap = mem_entries
        where   mem_names = ldapAttrYeah "memberNisNetgroup" entry
                mem_entries = map ( \n -> netgroup_from_map n ngmap ) mem_names

netgroup_members entry ngmap = []

netgroup_from_map ngname ngmapio =
    netgroupFromLDAP ngmapio (fromJust $ Map.lookup ngname ngmapio)

lookupNetgroupMap :: String -> IO (Map.Map String LDAPEntry) 
                            -> IO (Maybe LDAPEntry)
lookupNetgroupMap ngname ngmap =
                fmap (Map.lookup ngname) ngmap

lookupNetgroupMapYeah :: String -> IO (Map.Map String LDAPEntry)
                                -> IO LDAPEntry
lookupNetgroupMapYeah ngname ngmap =
                fmap fromJust (lookupNetgroupMap ngname ngmap)

mapFromNetgroup :: IO [LDAPEntry] -> IO (Map.Map String LDAPEntry)
mapFromNetgroup entries =
    fmap Map.fromList (fmap (map ( \ng -> (netgroup_name ng, ng))) entries)

netgroupMap :: IO (Map.Map String LDAPEntry)
netgroupMap = mapFromNetgroup searchNetgroupsInLDAP

build_netgroups_from_ldap =
    fmap (map $ netgroupFromLDAP []) searchNetgroupsInLDAP

{-
build_netgroups_from_ldap = do
    fmap (map netgroupFromLDAP ngmap) entries
    where
        entries = searchNetgroupsInLDAP
        ngmap = fmap mapFromNetgroup entries
-}
