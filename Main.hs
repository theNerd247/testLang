{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where

import Data.Functor.Identity
import Control.Monad (join)
import Control.Monad.Free
import Control.Monad.Free.TH

data Expr a = 
    Add a a
  | Mul a a
  | Num Integer
  deriving (Functor)

add :: (MonadFree Expr m) => m a -> m a -> m a
add a = join . liftF . Add a 

mul a = liftF . Mul a

num :: (MonadFree Expr m) => Integer -> m a
num = liftF . Num


type ExprM = Free Expr

runExprM :: (Num a) => ExprM a -> a
runExprM = runIdentity . iterM run where
  run (Add a b) = (+) <$> a <*> b
  run (Mul a b) = (*) <$> a <*> b
  run (Num i)   = return $ fromInteger i

main :: IO ()
main = putStrLn "Hello, Haskell!"
