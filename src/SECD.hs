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
  | ADD -- pop two args from the top of the stack and add them
  | MUL -- pop two args from the top of the stack and mul them
  | LEQ -- pop two args from the top of the stack and compare them
  | TRUE -- push True on the stack
  | FALSE -- push False on the stack
  | IF (Prog, Prog) -- pop an argument from the top of the stack and whether it it's True or False, evaluate the branh
  | CONS -- push the Cons, applied to top two elements of the stack on the stack
  | NIL -- push Nil constant on the stack
  | CASE (Prog, Prog) -- if Cons t1 t2 is on the stack, pop it and push t2 and t1 on the environment, evaluate c2
              -- if nil, pop it and evaluate c1
  | CLOS [SECDInstruction] [Int] -- closure operation? (which is what supposed to go on the stack?)
  deriving (Show, Eq)

-- code, env, stack
type SECDMachine = (Prog, Env, Prog)

compile :: DeBruijnLambda -> Prog
compile expr = case expr of
  (DVar     n  ) -> [ACCESS n] -- access at the index n
  (DLiteral lit) -> getLiteral lit
  (DAdd e1 e2  ) -> code e2 e1 ADD
  (DMul e1 e2  ) -> code e2 e1 MUL
  (DLEq e1 e2  ) -> code e2 e1 LEQ
  (DIf cond ifExp elseExp) ->
    compile cond ++ [IF (compile ifExp ++ [RET], compile elseExp ++ [RET])]
  (DCase cond c1 c2) ->
    compile cond ++ [CASE (compile c1 ++ [RET], compile c2 ++ [RET])]
  DNil          -> [NIL]
  (DCons e1 e2) -> compile e2 ++ compile e1 ++ [CONS]
  (DApp  e1 e2) -> code e1 e2 APP
  (DAbs term  ) -> [CLO (compile term ++ [RET])]
  _             -> undefined
 where
  code e1 e2 instr = compile e1 ++ compile e2 ++ [instr]
  getLiteral lit = case lit of
    (LInt  i) -> [CONST i]
    (LBool b) -> if b then [TRUE] else [FALSE]

-- -- a function that does one step
-- step :: SECDMachine -> SECDMachine
-- -- fill in this table
-- -- note: some type issues here, idk why
-- step ((CLO c) : prog, env, stack) = (c, env, CLO ((c, env) : stack))

-- -- fixPoint :: (Eq a) => (a -> a) -> a -> a
-- -- fixPoint f x = if (x == ) -- ?
