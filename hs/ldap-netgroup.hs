{-
 - LDAP.Netgroup module
 -}

module LDAP.Netgroup
( ldap_host
, ldap_port
, test_ldap_search
) where

import Data.Set as Set
import LDAP

data Netgroup = Netgroup { dn :: String
			 , netgroup :: String
			 , description :: Maybe String
			 , nisNetgroupTriple :: Set String
			 , memberNisNetgroup :: Set Netgroup
			 }
			deriving (Eq, Show)

instance Ord Netgroup where
	compare ng1 ng2 = compare (dn ng1) (dn ng2)

isFlatNetgroup :: Netgroup -> Bool
isFlatNetgroup ng = Set.null ( memberNisNetgroup ng )

-- flattenNetgroup :: Netgroup -> Netgroup
-- flattenNetgroup ng =

-- Local data
ldap_host = "localhost"

ldap_port :: LDAPInt
ldap_port = 389

basedn = Just "dc=pdx,dc=edu"

test_ldap_search lobj = ldapSearch lobj
			basedn
			LdapScopeSubtree
			(Just "cn=research")
			LDAPAttrList [ "cn"
				, "description"
				, "nisNetgroupTriple"
				, "memberNisNetgroup"
				]
			False

ldap_setup = do
	lconn <- ldapInit ldap_host ldap_port
	ldapSimpleBind lconn "" ""
	return lconn

-- Some test data
x = Netgroup { 	dn="zz"
		, netgroup="zz"
		, LDAP.Netgroup.description = Nothing
		, nisNetgroupTriple = Set.empty
		, memberNisNetgroup = Set.empty
		}

a = Netgroup { 	dn="aa"
		, netgroup="aa"
		, LDAP.Netgroup.description=Nothing
		, nisNetgroupTriple = Set.empty
		, memberNisNetgroup=Set.empty
		}

y = Netgroup { 	dn = "yy"
		, netgroup = "yy"
		, LDAP.Netgroup.description = Nothing
		, nisNetgroupTriple = Set.empty
		, memberNisNetgroup = Set.fromList [x,a]
		}

