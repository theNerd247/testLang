{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Syntax 
  ( ExprM
  , num
  , add
  , mul
  )
where

import Control.Monad.Free
import Control.Monad (join)
import Data.List.NonEmpty
import Numeric.Natural

data Expr a =
    Num Natural
  | Add a a 
  | Mul a a
  deriving (Functor)

type ExprM = Free Expr

num :: Natural -> ExprM a
num = liftF . Num

add :: (MonadFree Expr m) => m a -> m a -> m a
add a b = join $ liftF $ Add a b

mul :: (MonadFree Expr m) => m a -> m a -> m a
mul a b = join $ liftF $ Mul a b
