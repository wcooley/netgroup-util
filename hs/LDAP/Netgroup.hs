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
ldap_host = "localhost"

ldap_port :: LDAPInt
ldap_port = 389

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
    

searchNetgroupsInLDAP = do
                    lconn <- ldapAnonSetup
                    results <- search_ldap_netgroup lconn
                    return results


-- Attribute extractors to go along with Netgroup records
-- "cn" can be multivalued but should always exist
netgroup_name      = ldapAttr1Yeah "cn"

-- "description" might be multivalued; not sure; seems to be missing from my
-- installed schema??
netgroup_description = ldapAttr1 "description"
netgroup_triples = ldapAttrYeah "nisNetgroupTriple"

netgroup_members entry = []
netgroup_members_ entry = ldapAttr "memberNisNetgroup"

netgroupFromLDAP entry =
        Netgroup {  netgroup        = netgroup_name entry,
                    description     = netgroup_description entry,
                    netgroupTriples = netgroup_triples entry,
                    memberNetgroups = netgroup_members entry
                 }

build_netgroups_from_ldap = do
    fmap (map netgroupFromLDAP) searchNetgroupsInLDAP
