{-
 - t_Netgroup.hs - Tests for Netgroup module
 -}

import Netgroup
import Test.HUnit

barHost = "(bar,-,-)"
fooHost = "(foo,-,-)"

-- Some test data

a = Netgroup { 	netgroup = "aa"
		, description = Nothing
		, netgroupTriples = []
		, memberNetgroups = [b]
		}

b = Netgroup {	netgroup = "bb"
		, description = Just "The 'bb' netgroup"
		, netgroupTriples = [fooHost, barHost]
		, memberNetgroups = []
		}

c = Netgroup {  netgroup = "cc"
		, description = Nothing
		, netgroupTriples = []
		, memberNetgroups = [d,a]
		}

d = Netgroup { 	netgroup="dd"
		, description = Nothing
		, netgroupTriples = []
		, memberNetgroups = []
		}

cyc = Netgroup { netgroup = "cyc"
                , description = Just "Cyclical Netgroup"
                , netgroupTriples = []
                , memberNetgroups = [cyc]
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

tests = TestList [isflatgr_tests, innetgr_tests]

main = runTestTT tests
