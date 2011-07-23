module Compile where

import qualified HighLevel as HL
import qualified LowLevel as LL

compile :: HL.HLProg -> LL.LLProg
compile (HLP dts e) = compileExpr $ (foldr (.) id $ map compileDT dts) e

compileDT :: HL.DataType -> (HL.Expr -> HL.Expr)
compileDT = undefined

compileExpr :: HL.Expr -> LL.Prog
compileExpr = undefined
