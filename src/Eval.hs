{-# LANGUAGE UnicodeSyntax #-}

module Eval where

import Syntax

eval :: Expr -> Expr
eval Tr         = Tr
eval Fl         = Fl
eval Zero       = Zero
eval (Succ e)   = Succ (eval e)
eval (Pred e)   = Pred (eval e)
eval (If i t e) =
  case (eval i) of
    Tr → eval t
    _  → eval e
