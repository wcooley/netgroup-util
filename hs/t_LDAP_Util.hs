{-
 - t_LDAP_Util - Tests for LDAP.Util
 -}

import Data.Maybe (fromJust)
import LDAP
import LDAP.Util
import Test.HUnit

-- FIXME: This is type [LDAPEntry], not IO [LDAPEntry]... How can I make that,
-- for more accurate testing?
-- test_ldap_results :: IO [LDAPEntry]
test_ldap_results = [
    LDAPEntry {ledn = "cn=mail,ou=Netgroup,dc=pdx,dc=edu",
        leattrs = [ ("nisNetgroupTriple",["(agnar.oit.pdx.edu,-,)"]),
                    ("cn",["mail"])]
    },
    LDAPEntry {ledn = "cn=maillist,ou=Netgroup,dc=pdx,dc=edu",
        leattrs = [ ("nisNetgroupTriple",
                        ["(hem.oit.pdx.edu,-,)",
                        "(stheno.oit.pdx.edu,-,)"]),
                    ("cn",["maillist"])]
    },
    LDAPEntry {ledn = "cn=mailgw,ou=Netgroup,dc=pdx,dc=edu",
        leattrs = [ ("nisNetgroupTriple",
                        ["(fafnir.oit.pdx.edu,-,)",
                        "(hanuman.oit.pdx.edu,-,)",
                        "(hati.oit.pdx.edu,-,)",
                        "(nithog.oit.pdx.edu,-,)",
                        "(njord.oit.pdx.edu,-,)"]),
                    ("cn",["mailgw"])]
    },
    LDAPEntry {ledn = "cn=mon,ou=Netgroup,dc=pdx,dc=edu",
        leattrs = [ ("nisNetgroupTriple",
                        ["(rheya.oit.pdx.edu,-,)",
                        "(skrymir.oit.pdx.edu,-,)",
                        "(brunhilde.oit.pdx.edu,-,)",
                        "(mon.oit.pdx.edu,-,)",
                        "(newmon.oit.pdx.edu,-,)"]),
                    ("cn",["mon"])]
    },
    LDAPEntry {ledn = "cn=mgmt,ou=Netgroup,dc=pdx,dc=edu",
        leattrs = [ ("nisNetgroupTriple",
                        ["(andvari.oit.pdx.edu,-,)",
                        "(lindworm.oit.pdx.edu,-,)",
                        "(askr.oit.pdx.edu,-,)",
                        "(dromi.oit.pdx.edu,-,)",
                        "(midgard.oit.pdx.edu,-,)",
                        "(hoenir.oit.pdx.edu,-,)",
                        "(laga.oit.pdx.edu,-,)",
                        "(nott.oit.pdx.edu,-,)"]),
                    ("cn",["mgmt"])]
    },
    LDAPEntry {ledn = "cn=muppet,ou=Netgroup,dc=pdx,dc=edu",
        leattrs = [ ("cn",["muppet"]),
                    ("memberNisNetgroup",
                        ["mgmt", "mon"])]
    }
    ]

ldapattr_tests =  TestLabel "Tests of ldapAttr* functions" $ TestList [
        TestCase (assertEqual "single-valued attr"
                        ["mail"]
                        ( fromJust $ ldapAttr "cn" $ test_ldap_results !! 0 )),
        TestCase (assertEqual "multi-valued attr"
                        ["mgmt", "mon"]
                        ( fromJust $ ldapAttr "memberNisNetgroup" $ test_ldap_results !! 5 ))
    ]

tests = TestList [ ldapattr_tests ]

main = runTestTT tests
