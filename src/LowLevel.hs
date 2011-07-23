module LowLevel where

import Data.Binary

type Arity = Int
data LLProg = LLP [Fn] Expr
            deriving Show
data Fn = Fn Int Expr
        deriving Show
data Expr = Var Int
          | App Expr [Expr]
          | Prim Prim
          | Fun Arity Int
          deriving Show
data Prim = Plus
          | Minus
          | Div
          | Times
          | Num Int
          | Ifz
          deriving Show

{-
instance Binary LLProg where
  put (LLP fns expr) = do put fns
                          put expr
  get = do fns  <- get
           expr <- get
           return $ LLP fns expr

instance Binary Fn where
  put (Fn n lets body) = do put n
                            put lets
                            put body
  get = do n    <- get
           lets <- get
           body <- get
           return $ Fn n lets body

instance Binary Expr where
  put (Var n)    = do put (0 :: Word8)
                      put n
  put (App e es) = do put (1 :: Word8)
                      put e
                      put es
  put (Prim p)   = do put (2 :: Word8)
                      put p
  get = do v <- get
           case (v :: Word8) of
             0 -> fmap Var get
             1 -> do e  <- get
                     es <- get
                     return $ App e es
             2 -> fmap Prim get

instance Binary Prim where
  put Plus = put (0 :: Word8)
  put Minus = put (1 :: Word8)
  put Div = put (2 :: Word8)
  put Times = put (3 :: Word8)
  put (Num n) = do put (4 :: Word8)
                   put n
  get = do v <- get
           case (v :: Word8) of
             0 -> return Plus
             1 -> return Minus
             2 -> return Div
             3 -> return Times
             4 -> do n <- get
                     return $ Num n
-}
