{-
 - LDAP.Netgroup module
 -}

module LDAP.Netgroup
( ldap_host
, ldap_port
, test_ldap_search
) where

import qualified Data.Map as Map
import Data.Maybe
import LDAP hiding (description)
import Netgroup

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


ldap_getattr :: String -> LDAPEntry -> Maybe [String]
ldap_getattr attr entry = 
            case filter_attr of
                    [] -> Nothing
                    _  -> Just (snd $ filter_attr !! 0)
            where filter_attr =
                    filter (\x -> fst x == attr)
                        (leattrs entry)

-- "cn" can by multivalued but should always exist
getng_name      = head . fromJust . ldap_getattr "cn"
-- "description" might be multivalued; not sure
getng_description x = Nothing -- maybe Nothing head (ldap_getattr "description" x)
getng_triples = (fromMaybe []) . ldap_getattr "nisNetgroupTriple"
getng_members entry = []

getng_members_ entry = ldap_getattr "memberNisNetgroup"


-- get_netgroups_from_ldap :: IO [LDAPEntry]
get_netgroups_from_ldap = do
                    lconn <- ldap_setup
                    results <- search_ldap_netgroup lconn
                    return results

build_netgroup_from_ldap entry =
        Netgroup {  netgroup = getng_name entry, 
                    description = getng_description entry,
                    netgroupTriples = getng_triples entry,
                    memberNetgroups = getng_members entry
                 }

build_netgroups_from_ldap = do
    fmap (map build_netgroup_from_ldap) get_netgroups_from_ldap
