{- |
Module      :  $Header$ 
Description :  Supports generation of graphs in Graphviz's dot-language.
Copyright   :  (c) Sascha Fendrich
License     :  GPL-3

Maintainer  :  sascha.fendrich@uni-bamberg.de
Stability   :  experimental
Portability :  portable 

The Graphviz module supports the generation of graphs in the dot-language
used by the Graphviz graph visualization tools. 
-}

module Graphviz where

import Data.List

class Dotable a where
  toDot :: a -> String

instance Dotable Int where
  toDot i = show i

-- make a node from name and options
makeNode :: String -> [String] -> String
makeNode name options = "\"" ++ name ++"\" [" ++ intercalate "," options ++ "]\n"

-- make an arrow from source, target and options
makeArrow :: String -> String -> [String] -> String
makeArrow source target options =
  "\"" ++ source ++ "\"->\"" ++ target ++ "\" [" ++ intercalate "," options ++ "]\n"

-- make a labelled arrow
simpleArrow :: String -> String -> String -> [String] -> String
simpleArrow s l t opts =
  makeArrow s t (("label=\""++ l ++ "\""):opts)

-- make a labelled multiarrow
multiArrow :: String -> String -> [String] -> [String] -> String
multiArrow s l ts opts = (makeNode dummy ["shape=point"])
  ++ (makeArrow s dummy ("arrowhead=none":("label=\"" ++ l ++ "\""):opts))
  ++ (concatMap (\t->makeArrow dummy t opts) ts)
  where dummy = intercalate "-" (s:l:ts)

