{-
 - t_LDAP_Util - Tests for LDAP.Util
 -}

import Data.Maybe (fromJust)
import LDAP
import LDAP.Util
import LDAP.Test_data
import Test.HUnit

ldapattr_tests =  TestLabel "Tests of ldapAttr* functions" $ TestList [
        TestCase (assertEqual "ldapAttr: single-valued"
                        (Just ["mail"])
                        ( ldapAttr "cn" $ test_ldap_results !! 0 )),
        TestCase (assertEqual "ldapAttr: multi-valued"
                        (Just ["mgmt", "mon"])
                        ( ldapAttr "memberNisNetgroup" $
                                    test_ldap_results !! 5 )),
        TestCase (assertEqual "ldapAttrYeah: single-valued"
                        ["mail"]
                        ( ldapAttrYeah "cn" $ test_ldap_results !! 0 )),
        TestCase (assertEqual "ldapAttrYeah: multi-valued"
                        ["mgmt", "mon"]
                        ( ldapAttrYeah "memberNisNetgroup" $
                                    test_ldap_results !! 5 )),
        TestCase (assertEqual "ldapAttr1: multi-valued"
                        (Just "mgmt")
                        ( ldapAttr1 "memberNisNetgroup" $
                                    test_ldap_results !! 5 )),
        TestCase (assertEqual "ldapAttr1Yeah: multi-valued"
                        "mgmt"
                        ( ldapAttr1Yeah "memberNisNetgroup" $
                                    test_ldap_results !! 5 )),
        TestCase (assertEqual "ldapAttr, non-existent attr"
                        Nothing
                        ( ldapAttr "nonExistentAttr" $
                                    test_ldap_results !! 5 )),
        TestCase (assertEqual "ldapAttrYeah, non-existent attr"
                        []
                        ( ldapAttrYeah "nonExistentAttr" $
                                    test_ldap_results !! 5 )),
        TestCase (assertEqual "ldapAttr1Yeah, non-existent attr"
                        ""
                        ( ldapAttr1Yeah "nonExistentAttr" $
                                    test_ldap_results !! 5 ))

    ]

tests = TestList [ ldapattr_tests ]

main = runTestTT tests
