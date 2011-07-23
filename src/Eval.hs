module Eval where

import LowLevel

primArity :: Prim -> Arity
primArity (Num _) = 0
primArity Ifz = 3
primArity _ = 2

arity :: Expr -> Arity
arity (Prim p) = primArity p
arity (Fun i _) = i
arity _ = 0

isWHNF :: Expr -> Bool
isWHNF (Var _) = False
isWHNF (App e args) = arity e > length args
isWHNF _ = True

getFun :: LLProg -> Int -> Fn
getFun (LLP fns _) i = fns !! i

subst :: [Expr] -> Expr -> Expr
subst args (Var n) = args !! n
subst args (App e es) = App (subst args e) (map (subst args) es)
subst _ e = e


cleanup :: Expr -> Expr
cleanup (App e []) = e
cleanup e = e

getPrim :: Prim -> (Int -> Int -> Int)
getPrim Plus = (+)
getPrim Minus = (-)
getPrim Times = (*)
getPrim Div = div

stepPrim prog (Num _) _ = error "can't step Num" 
stepPrim prog Ifz [ Prim (Num 0), e, _ ] = e
stepPrim prog Ifz [ Prim (Num _), _, e ] = e
stepPrim prog p [ Prim (Num n), Prim (Num m) ] =
  Prim $ Num $ (getPrim p) n m
stepPrim prog p [ Prim (Num n), e ] =
  App (Prim p) [ Prim (Num n), step prog e ]
stepPrim prog p [ e, e' ] = App (Prim p) [ step prog e, e' ]

step prog expr =
  case expr of
    App e es | not $ isWHNF e -> App (step prog e) es
    App (App e es) es' -> App e (es ++ es')
    App (Fun _ i) es ->
      cleanup $ App (subst es f) (drop n es)
      where (Fn n f) = getFun prog i
    App (Prim p) es -> stepPrim prog p es

eval prog expr =
  if isWHNF expr then expr else eval prog $ step prog expr
