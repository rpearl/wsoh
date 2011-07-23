module HighLevel where

import LowLevel (Prim)

data HLProg = HLP [DataType] Expr

data DataType = DT [(String, [DataType])]
              | Lit

type Var = String
data Expr = Case [(String, Expr)]
          | Lambda [Var] [Expr]
          | Prim Prim
          | App Expr Expr
          | Let Var Expr Expr
