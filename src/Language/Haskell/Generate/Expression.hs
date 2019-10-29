module Language.Haskell.Generate.Expression 
  ( Expression(..)
  , app
  ) where

import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax

newtype Expression t = Expression { runExpression :: Exp SrcLoc }

app :: SrcLoc -> Expression (a -> b) -> Expression a -> Expression b
app l (Expression a) (Expression b) = Expression $ App l a b

