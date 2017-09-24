{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Parser.AST where

import           Control.Monad.Free
import qualified Control.Monad.Trans.Free as F
import           Data.Functor.Foldable
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List.NonEmpty
import Data.Monoid

type Symbol = LBS.ByteString

data Exp_ a
  = Define (NonEmpty a) a
  | Lambda (NonEmpty a) a
  | List [a]
  | Quote a
  | Sym Symbol 
  | I Int
  | B Bool
    deriving (Show, Functor, Foldable, Traversable)

type Exp = Fix Exp_

newtype PrintExp = PrintExp Exp

printSeq = unwords . toList

instance Show PrintExp where
  show (PrintExp e) = cata f e
    where f (Define xs e) = "(define (" <> printSeq xs <> ") " <> e <> ")"
          f (Lambda xs e) = "(lambda (" <> printSeq xs <> ") " <> e <> ")"
          f (List xs) = "(" <> unwords xs <> ")"
          f (Quote e) = '\'' : e
          f (Sym s) = unpack s
          f (I i) = show i
          f (B b) = show b 



