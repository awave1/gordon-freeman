module SECD where

import           Syntax

type Prog = [SECDInstruction]
type Env = [Int]

data SECDInstruction
  = CLO Prog -- push closure of code with current env to the stack
  | APP -- popp function closure and argument and perform application
  | ACCESS Int -- push nth val in the env to the stack
  | RET -- return the top value on the stack and jump to the next on the stack
  | CONST Int -- constant int val
  | ADD
  | MUL
  | LEQ
  | TRUE
  | FALSE
  | IF
  | CONS
  | NIL -- List
  | CASE Prog -- List
  | CLOS [SECDInstruction] [Int] -- closure operation? (which is what supposed to go on the stack?)
  deriving (Show, Eq)

-- code, env, stack
type SECDMachine = (Prog, Env, Prog)

compile :: DeBruijnLambda -> Prog
compile expr = case expr of
  (DAbs term ) -> CLO (compile term) : [RET]
  (DApp t1 t2) -> compile t1 ++ compile t2 ++ [APP]
  (DVar n    ) -> [ACCESS n]
  _            -> undefined

-- -- a function that does one step
-- step :: SECDMachine -> SECDMachine
-- -- fill in this table
-- -- note: some type issues here, idk why
-- step ((CLO c) : prog, env, stack) = (c, env, CLO ((c, env) : stack))

-- -- fixPoint :: (Eq a) => (a -> a) -> a -> a
-- -- fixPoint f x = if (x == ) -- ?
