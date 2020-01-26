{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax where

type Name = String

data Expr =
    Var Name 
  | App Expr Expr
  | Lam Name Expr
  | Lit Lit
  deriving (Show, Eq)

data Lit =
    LInt Int
  | LBool Bool
  deriving (Show, Eq)
