{-# OPTIONS_GHC "-Wno-orphans" #-}
{-# LANGUAGE UndecidableInstances #-} -- for apply collection instance

module Common where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Text.PrettyPrint qualified as Pretty
import Text.PrettyPrint.GenericPretty
import Data.Coerce
import Data.Generics.Uniplate.Data (Uniplate)
import Data.Generics.Uniplate.Data qualified as Uniplate
import Optics

instance (Out k, Out v) => Out (Map k v) where
  doc = doc . Map.toList
  docPrec prec x = docPrec prec (Map.toList x)

instance (Out a, Ord a) => Out (Set a) where
  doc = doc . Set.toAscList
  docPrec prec x = docPrec prec (Set.toAscList x)

newtype OutShow a = OutShow a
instance Out a => Show (OutShow a) where
  show (OutShow x) = pretty x

newtype ShowOut a = ShowOut a
instance Show a => Out (ShowOut a) where
  doc (ShowOut x) = Pretty.text (show x)
  docPrec = const doc

subTrees :: Uniplate a => Fold a a
subTrees = to Uniplate.universe % folded

toSetOf :: (Is k A_Fold, Ord a) => Optic' k is s a -> s -> Set a
toSetOf o = o `foldMapOf` Set.singleton

class From from to where
  from :: from -> to

class Apply f arg res | f arg -> res where
  (@) :: f -> arg -> res

instance {-# OVERLAPPABLE #-} (Apply f arg res, Functor collection) => Apply f (collection arg) (collection res) where
  f @ args = fmap (f @) args
