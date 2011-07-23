module HighLevel where

import LowLevel (Prim)

data HLProg = HLP [DataType] Expr

data DataType = DT [(String, Int)]

type Var = String
data Expr = Case Expr [(String, Expr)]
          | Lambda [Var] Expr
          | Prim Prim
          | App Expr [Expr]
          | Let Var Expr Expr
          | Var Var deriving (Ord, Eq)
