module HighLevel where

import LowLevel (Prim)

data HLProg = HLP [DataType] Expr

data DataType = DT String [String]

type Var = String
data Expr = Case [(String, (Int, Expr))]
          | Lambda [Var] [Expr]
          | Prim Prim
          | App [Expr]
          | Let Var Expr Expr
          | Var Var
