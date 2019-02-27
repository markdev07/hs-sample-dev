module X86Sem where

import Ast
import AstContext (getOperandAst)

import           Hapstone.Capstone
import           Hapstone.Internal.Capstone as Capstone
import           Hapstone.Internal.X86      as X86
import           Util
import           AstContext


--Tests
import Test.HUnit

--ll, ml, hl



--  test for add_S
test_add_s :: Test
test_add_s = TestCase (assertEqual "for (add_s XXX)," (Y) (add_s XXX))



--  test for sub_s
test_sub_s :: Test
test_sub_s = TestCase (assertEqual "for (sub_s XXX)," (Y) (sub_s XXX))



--  test for xor_s
test_xor_s :: Test
test_xor_s = TestCase (assertEqual "for (xor_s XXX)," (Y) (xor_s XXX))



--  test for push
test_push :: Test
test_push = TestCase (assertEqual "for (push XXX)," (Y) (push XXX))



--  test for pop
test_pop :: Test
test_pop = TestCase (assertEqual "for (pop XXX)," (Y) (pop XXX))



tests = TestList [
                  TestLabel "test_add_s" test_add_s
                , TestLabel "test_sub_s" test_sub_s
                , TestLabel "test_xor_s" test_xor_s
                --list can continue
              ]
main :: IO Counts
main = do _ <- runTestTT tests