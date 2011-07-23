module Compile where

import qualified HighLevel as HL
import qualified LowLevel as LL
import Data.List
import Control.Monad.State

composer :: (b -> a -> a) -> [b] -> a -> a
composer f = foldr ((.) . f) id

mangle :: String -> String
mangle = (' ' :)

compile :: HL.HLProg -> LL.LLProg
compile (HL.HLP dts e) = compileExprFull $ composer compileDT dts e

compileDT :: HL.DataType -> HL.Expr -> HL.Expr
compileDT (HL.DT arms) =
  composer (compileArm $ length arms) $ zip [1..] $ sort arms

compileArm :: Int -> (Int, (String, Int)) -> HL.Expr -> HL.Expr
compileArm z (n, (name, l)) =
   let vars = makeVars z n
   in HL.Let (mangle name) (HL.Lambda (internal ++ vars)
                                      (HL.App (HL.Var useful)
                                              (map HL.Var internal)))
   where nameMangle x = name ++ " " ++ x
         nameMangle'  = mangle . nameMangle
         useful = nameMangle "special"
         makeVars z n = map nms [1..(n - 1)] ++
                        [useful] ++
                        map nms [(n + 1)..z]
           where nms = nameMangle . show
         internal = map (nameMangle' . show) [1..l]

type FnEmit = State [LL.Fn]

compileExprFull :: HL.Expr -> LL.LLProg
compileExprFull x =
  let (e, s) = runState (compileExpr x) []
  in LL.LLP (reverse s) e

compileExpr :: HL.Expr -> FnEmit LL.Expr
compileExpr (HL.Case e branches') =
   compileExpr (HL.App e (map snd $ sort branches'))
compileExpr (HL.Lambda vars_ e_) = undefined
compileExpr (HL.Prim p) = return $ LL.Prim p
compileExpr (HL.App e es) = do
   (e' : es') <- mapM compileExpr $ e : es
   return $ LL.App e' es'
compileExpr (HL.Let v b e) = compileExpr $ HL.App (HL.Lambda [v] e) [b]
compileExpr (HL.Var var) = error "Unbound Variable"
