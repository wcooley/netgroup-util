{-
 - Tests for LDAP.Netgroup
 -}

import LDAP
import LDAP.Netgroup
import LDAP.Test_data
import Netgroup
import Test.HUnit

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

tests = TestLabel "LDAP.Netgroup tests" $ TestList []
main = runTestTT tests
