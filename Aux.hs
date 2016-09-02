{-
Auxilary functions and data types for Gemia

Copyright (C) 2015 Sascha Fendrich

This file is part of Gemia.

Gemia is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Gemia is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Gemia.  If not, see <http://www.gnu.org/licenses/>.
-}

{-|
Module      :  $Header$ 
Description :  General auxilary stuff for Gemia.
Copyright   :  Sascha Fendrich
License     :  GPL-3
Maintainer  :  sascha.fendrich@uni-bamberg.de
Stability   :  experimental
Portability :  portable 

The Aux module declares some general stuff needed in Gemia, like
binary trees and making pairs.
-}

module Aux where

import Graphviz

-- Binary trees
data BTree a = Leaf a | Branch (BTree a) (BTree a) deriving (Eq,Show)

instance (Ord a) => Ord (BTree a) where
  compare (Leaf a) (Leaf b) = compare a b
  compare (Branch a1 a2) (Branch b1 b2) 
    | compare a1 b1 == LT = LT
    | compare a1 b1 == EQ = compare a2 b2
    | otherwise           = GT

instance (Dotable n) => Dotable (BTree n) where
  toDot (Leaf a) = toDot a
  toDot (Branch a b) = "(" ++ (toDot a) ++ "," ++ (toDot b) ++ ")"

-- List of pairs
makePairsBy :: (a -> b -> c) -> [a] -> [b] ->[c]
makePairsBy f as bs = [f a b|a<-as,b<-bs]


