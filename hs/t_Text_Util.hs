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

subst_tests = TestLabel "Tests for subst" $
            TestList [
                TestCase (assertEqual "single subst test"
                            "abc_def"
                            (subst '-' '_' "abc-def"))
                , TestCase (assertEqual "multiple subst test"
                            "abc*def*ghi*jkl"
                            (subst '0' '*' "abc0def0ghi0jkl"))
            ]

tests = TestLabel "Text.Util tests" $
            TestList [ x_tests
                    , quot_tests
                    , subst_tests
            ]

main = runTestTT tests
