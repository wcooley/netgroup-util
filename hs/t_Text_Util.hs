{-
 - t_Text_Util - Tests for Text.Util
 -}

import Test.HUnit
import Text.Util

join_tests = TestLabel "Text.Util.join tests" $
            TestList [
                TestCase (assertEqual "join \":\" words"
                            "a:b:c"
                            (join ":" ["a","b","c"])
                        )
            ]

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
            TestList [ join_tests
                    , x_tests
                    , quot_tests
            ]

main = runTestTT tests
