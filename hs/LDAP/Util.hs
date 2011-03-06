{-
 - LDAP.Util module
 -}

module LDAP.Util where

import Data.Maybe (fromMaybe)
import LDAP

ldap_host = "localhost"

ldap_port :: LDAPInt
ldap_port = 389

-- Pick out a list values for an attribute in an LDAPEntry
ldapAttr :: String -> LDAPEntry -> Maybe [String]
ldapAttr attr entry =
            case filter_attr of
                    [] -> Nothing
                    _  -> Just (snd $ filter_attr !! 0)
            where filter_attr =
                    filter (\x -> fst x == attr)
                        (leattrs entry)

-- Same, but only the first of a multi-valued attribute
ldapAttr1 :: String -> LDAPEntry -> Maybe String
ldapAttr1 attr entry = fmap head (ldapAttr attr entry)

-- Likewise, but take it out of the Maybe ([] for Nothing)
ldapAttrYeah :: String -> LDAPEntry -> [String]
ldapAttrYeah attr entry = fromMaybe [] $ ldapAttr attr entry

-- Likewise, but "" for Nothing
ldapAttr1Yeah :: String -> LDAPEntry -> String
ldapAttr1Yeah attr entry =  case res1 of
                                [] -> ""
                                otherwise -> head res1
            where res1 = ldapAttrYeah attr entry

-- Quick & easy init & anonymous bind
ldapAnonSetup = do
	lconn <- ldapInit ldap_host ldap_port
	ldapSimpleBind lconn "" ""
	return lconn
