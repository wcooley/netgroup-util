{-
 - t_Text_Util - Tests for Text.Util
 -}

import Test.HUnit
import Text.Util

x_tests = TestLabel "Text.Util.x tests" $
            TestList [
                TestCase (assertEqual "x: \"*\" `x` 4"
                            "****"
                            ("*" `x` 4)
                        )
                , TestCase (assertEqual "x: 0"
                            ""
                            ("!" `x` 0)
                        )
            ]

quot_tests = TestLabel "Tests for dquot and squote" $
            TestList [
                TestCase (assertEqual "dquot test"
                            "\"word\""
                            (dquot "word"))
                , TestCase (assertEqual "squot test"
                            "'word'"
                            (squot "word"))
            ]


tests = TestLabel "Text.Util tests" $
            TestList [ x_tests
                    , quot_tests
            ]

main = runTestTT tests
