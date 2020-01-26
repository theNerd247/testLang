{-# LANGUAGE UnicodeSyntax #-}

module Eval where

import Syntax
import Control.Monad (join)
import qualified Data.Map as M

data Value = 
    VInt Int
  | VBool Bool
  | VClosure Scope String Expr 

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
eval s (Lam x body)    = return $ VClosure s x body
eval s (App a b)       = join $ apply <$> (eval s a) <*> (eval s b)

apply :: Value -> Value -> EvalM Value
apply (VClosure s n t1) t2 = eval (extend s n t2) t1
apply _ _ = Left "Not an appliable closure"

extend :: Scope -> Name -> Value -> Scope
extend s n v = M.insert n v s

emptyScope :: Scope
emptyScope = M.empty
