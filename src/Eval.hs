{-# LANGUAGE UnicodeSyntax #-}

module Eval where

import Syntax
import qualified Data.Map as M

data Value = 
    VInt Int
  | VBool Bool
  | VClosure String Expr Scope

instance Show Value where
  show (VInt x)   = show x
  show (VBool x)  = show x
  show VClosure{} = "<<closure>>"

type Scope = M.Map Name Value

type EvalM a = Either String a

runEval :: Expr -> Either String Value
runEval = eval emptyScope

eval :: Scope -> Expr -> EvalM Value
eval _ (Lit (LInt n))  = return $ VInt n
eval _ (Lit (LBool b)) = return $ VBool b
eval s (Var x)         = maybe (Left $ "not in scope: " ++ x) return (M.lookup x s)
eval s (Lam x body)    = return $ VClosure x body s
eval _ _               = Left "not implemented"

emptyScope :: Scope
emptyScope = M.empty
