{-# LANGUAGE UnicodeSyntax #-}

module Eval where

import Syntax

eval :: Expr -> Expr
eval Tr = Tr
eval Fl = Fl
eval Zero = Zero
eval Succ 
