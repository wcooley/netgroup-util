{-
 - LDAP.Util module
 -}

module LDAP.Util where

import LDAP


ldap_getattr :: String -> LDAPEntry -> Maybe [String]
ldap_getattr attr entry =
            case filter_attr of
                    [] -> Nothing
                    _  -> Just (snd $ filter_attr !! 0)
            where filter_attr =
                    filter (\x -> fst x == attr)
                        (leattrs entry)
