{-
 - t_Netgroup.hs - Tests for Netgroup module
 -}

import Netgroup
import Test.HUnit

barHost = "(bar,-,-)"
fooHost = "(foo,-,-)"

-- Some test data

a = Netgroup { 	netgroup = "aa"
		, netgroupTriples = []
		, memberNetgroups = ["bb"]
		}

b = Netgroup {	netgroup = "bb"
		, netgroupTriples = [fooHost, barHost]
		, memberNetgroups = []
		}

c = Netgroup {  netgroup = "cc"
		, netgroupTriples = []
		, memberNetgroups = ["dd","aa"]
		}

d = Netgroup { 	netgroup="dd"
		, netgroupTriples = []
		, memberNetgroups = []
		}

cyc = Netgroup { netgroup = "cyc"
                , netgroupTriples = []
                , memberNetgroups = ["cyc"]
                }

isflatgr_tests = TestLabel "isFlatNetgroup Tests" $ TestList [
        TestCase (assertEqual "netgroup is flat"
                    True
                    (isFlatNetgroup b)
                ),
        TestCase (assertEqual "netgroup is nested"
                    False
                    (isFlatNetgroup c)
                )
    ]

innetgr_tests = TestList []
{-
innetgr_tests = TestLabel "inNetgroup Tests" $ TestList [
        TestCase (assertEqual "triple immediately contained in netgroup"
                    True
                    (barHost `inNetgroup` b)
                ),
        TestCase (assertEqual "triple not contained in netgroup"
                    False
                    (barHost `inNetgroup` d)
                ),
        TestCase (assertEqual "triple contained in first member netgroup"
                    True
                    (barHost `inNetgroup` a)
                ),
        TestCase (assertEqual "triple in tail of member netgroups"
                    True
                    (barHost `inNetgroup` c)
                )
    ]
-}

parsetripl_tests = TestLabel "parseNetgroupTriple tests" $ TestList [
        TestCase (assertEqual "null triple"
                    ("","","")
                    (parseNetgroupTriple "(,,)")
                )
        , TestCase (assertEqual "dashed triple"
                    ("-","-","-")
                    (parseNetgroupTriple "(-,-,-)")
                )
        , TestCase (assertEqual "host-triple"
                    ("odin.pdx.edu", "-", "")
                    (parseNetgroupTriple "(odin.pdx.edu,-,)")
                )
        , TestCase (assertEqual "user-triple"
                    ("-", "wcooley", "")
                    (parseNetgroupTriple "(-,wcooley,)")
                )
    ]

tests = TestList [isflatgr_tests, innetgr_tests, parsetripl_tests]

main = runTestTT tests
