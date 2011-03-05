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

search_ldap_netgroup lobj =
                        ldapSearch lobj
                        basedn
                        LdapScopeSubtree
                        (Just "(&(objectclass=nisNetgroup)(cn=b*))")
			(LDAPAttrList [ "cn"
				, "description"
				, "nisNetgroupTriple"
				, "memberNisNetgroup"
				])
			False
    

ldap_setup = do
	lconn <- ldapInit ldap_host ldap_port
	ldapSimpleBind lconn "" ""
	return lconn


-- "cn" can by multivalued but should always exist
netgroup_name      = head . fromJust . ldapAttr "cn"
-- "description" might be multivalued; not sure; seems to be missing from my
-- installed schema??
netgroup_description x = Nothing -- maybe Nothing head (ldapAttr "description" x)
netgroup_triples = (fromMaybe []) . ldapAttr "nisNetgroupTriple"
netgroup_members entry = []

netgroup_members_ entry = ldapAttr "memberNisNetgroup"


-- get_netgroups_from_ldap :: IO [LDAPEntry]
get_netgroups_from_ldap = do
                    lconn <- ldap_setup
                    results <- search_ldap_netgroup lconn
                    return results

build_netgroup_from_ldap entry =
        (netgroup_name entry,
        Netgroup {  netgroup        = netgroup_name entry,
                    description     = netgroup_description entry,
                    netgroupTriples = netgroup_triples entry,
                    memberNetgroups = netgroup_members entry
                 }
        )

build_netgroups_from_ldap = do
    fmap (map build_netgroup_from_ldap) get_netgroups_from_ldap
