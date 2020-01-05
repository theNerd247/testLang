{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax 
  ( ExprM
  , num
  , add
  , mul
  , printPretty
  , pretty
  )
where

import Control.Monad.Free
import Control.Monad (join)
import Data.List.NonEmpty
import Numeric.Natural
import Text.PrettyPrint hiding ((<>))
import Text.PrettyPrint.HughesPJClass
import Data.Functor.Classes (Show1)
import Control.Comonad (extract)

data Expr a =
    Num Natural
  | Add a a 
  | Mul a a
  deriving (Functor)

type ExprM = Free Expr

instance Pretty Natural where
  pPrint = pPrint . toInteger

num :: Natural -> ExprM a
num = liftF . Num

add :: (MonadFree Expr m) => m a -> m a -> m a
add a b = join $ liftF $ Add a b

mul :: (MonadFree Expr m) => m a -> m a -> m a
mul a b = join $ liftF $ Mul a b

printPretty :: (Pretty a) => ExprM a -> IO ()
printPretty = putStrLn . render . pretty

pretty :: (Pretty a) => ExprM a -> Doc
pretty = iter runPretty . fmap pPrint

runPretty :: Expr Doc -> Doc
runPretty (Num x)   = pPrint x
runPretty (Add a b) = "add" <+> a <+> b
runPretty (Mul a b) = "mul" <+> a <+> b
