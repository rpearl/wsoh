module Compile where

import qualified HighLevel as HL
import qualified LowLevel as LL

composer :: (b -> a -> a) -> [b] -> (a -> a)
composer f xs = foldr (.) id $ map f xs

mangle :: String -> String
mangle = (' ' :)

compile :: HL.HLProg -> LL.LLProg
compile (HL.HLP dts e) = compileExpr $ (composer compileDT dts) e

compileDT :: HL.DataType -> (HL.Expr -> HL.Expr)
compileDT (HL.DT arms) =
  composer (compileArm $ length arms) $ zip [1..] arms

compileArm :: Int -> (Int, (String, Int)) -> (HL.Expr -> HL.Expr)
compileArm z (n, (name, l)) =
  let vars = makeVars z n
  in HL.Let (mangle name) (HL.Lambda (internal ++ vars)
                                     (HL.App ((HL.Var useful) :
                                               (map HL.Var internal))))
  where nameMangle x = name ++ " " ++ x
        nameMangle'  = mangle . nameMangle
        useful = nameMangle "special"
        makeVars z n = (map nms [1..(n - 1)]) ++
                       [useful] ++
                       (map nms [(n + 1)..z])
          where nms = nameMangle . show
        internal = map (nameMangle' . show) [1..l]

compileExpr :: HL.Expr -> LL.LLProg
compileExpr = undefined
