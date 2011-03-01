{-
 - LDAP.Netgroup module
 -}

module LDAP.Netgroup
( ldap_host
, ldap_port
, test_ldap_search
) where

import LDAP hiding (description)
import Netgroup
import Data.Set as Set

-- Local data
ldap_host = "localhost"

ldap_port :: LDAPInt
ldap_port = 389

basedn = Just "dc=pdx,dc=edu"

test_ldap_search lobj = ldapSearch lobj
			basedn
			LdapScopeSubtree
			(Just "cn=research")
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
