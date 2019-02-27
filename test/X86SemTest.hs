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

add_s :: CsInsn -> [AstNodeType]
add_s inst =
  let op1 = getOperandAst $ get_first_opr_value inst
      op2 = getOperandAst $ get_second_opr_value inst
  in [
      BvaddNode op1 op2,
      SetFlag Adjust (AssertNode "Adjust flag unimplemented"),
      SetFlag Parity (AssertNode "Parity flag unimplemented"),
      SetFlag Sign (AssertNode "Sign flag unimplemented"),
      SetFlag Zero (AssertNode "Zero flag unimplemented"),
      SetFlag Carry (AssertNode "Carry flag unimplemented"),
      SetFlag Overflow (AssertNode "Overflow flag unimplemented")
    ]


--  test for add_S
test_add_s :: Test
test_add_s = TestCase (assertEqual "for (add_s XXX)," (Y) (add_s XXX))

---more test here



sub_s :: CsInsn -> [AstNodeType]
sub_s inst =
  let op1 = getOperandAst $ get_first_opr_value inst
      op2 = getOperandAst $ get_second_opr_value inst
  in [
      BvsubNode op1 op2,
      SetFlag Adjust (AssertNode "Adjust flag unimplemented"),
      SetFlag Parity (AssertNode "Parity flag unimplemented"),
      SetFlag Sign (AssertNode "Sign flag unimplemented"),
      SetFlag Zero (AssertNode "Zero flag unimplemented"),
      SetFlag Carry (AssertNode "Carry flag unimplemented"),
      SetFlag Overflow (AssertNode "Overflow flag unimplemented")
    ]

--  test for sub_s
test_sub_s :: Test
test_sub_s = TestCase (assertEqual "for (sub_s XXX)," (Y) (sub_s XXX))

---more test here

xor_s :: CsInsn -> [AstNodeType]
xor_s inst =
  let op1 = getOperandAst $ get_first_opr_value inst
      op2 = getOperandAst $ get_second_opr_value inst
  in [
      BvxorNode op1 op2,
      SetFlag Adjust (AssertNode "Adjust flag unimplemented"),
      SetFlag Parity (AssertNode "Parity flag unimplemented"),
      SetFlag Sign (AssertNode "Sign flag unimplemented"),
      SetFlag Zero (AssertNode "Zero flag unimplemented"),
      SetFlag Carry (AssertNode "Carry flag unimplemented"),
      SetFlag Overflow (AssertNode "Overflow flag unimplemented")
    ]

--  test for xor_s
test_xor_s :: Test
test_xor_s = TestCase (assertEqual "for (xor_s XXX)," (Y) (xor_s XXX))

---more test here




push ::  CsInsn -> [AstNodeType]
push inst =
  let op1 = getOperandAst $ get_first_opr_value inst
  in [
      SetReg stack_register (BvsubNode (GetReg stack_register) (BvNode 4 32)),
      Store (GetReg stack_register) op1
    ]


--  test for push
test_push :: Test
test_push = TestCase (assertEqual "for (push XXX)," (Y) (push XXX))

---more test here


pop ::  CsInsn -> [AstNodeType]
pop inst =
  --whenever the operation is a store reg or store mem depends on op1
  let
    read_exp = Read (BvaddNode (GetReg stack_register) (BvNode 4 32))
    pop_op = case (get_first_opr_value inst) of
              (Reg reg) -> (SetReg stack_register read_exp)
              (Mem mem) -> Store (getLeaAst mem) read_exp
              (Imm _) -> AssertNode "pop with imm, wtf"
  in
   [
      SetReg stack_register (BvaddNode (GetReg stack_register) (BvNode 4 32)),
      pop_op
    ]

--  test for pop
test_pop :: Test
test_pop = TestCase (assertEqual "for (pop XXX)," (Y) (pop XXX))

---more test here


--isolate x86 32 bit specific stuff so its easier to refactor later
stack_register :: Register
stack_register = (X86Reg X86RegEsp)


tests = TestList [
                  TestLabel "test_add_s" test_add_s
                , TestLabel "test_sub_s" test_sub_s
                , TestLabel "test_xor_s" test_xor_s
                --list can continue
              ]
main :: IO Counts
main = do _ <- runTestTT tests