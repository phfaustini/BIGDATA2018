{-# LANGUAGE  NoMonomorphismRestriction #-}

module Dados where

import Data.List (transpose, foldl1', sortBy, groupBy)
import Control.Parallel
import Control.Parallel.Strategies

type ChunksOf a = [a]

-- | operações de mapeamento e combinação
ordenaTupla :: (Ord a) => (a, t) -> (a, t) -> Ordering
ordenaTupla (a1,b1) (a2,b2)
  | a1 < a2 = LT
  | a1 > a2 = GT
  | a1 == a2  = EQ

ordenaTuplaVal (a1,b1) (a2,b2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | b1 == b2  = EQ
  
agrupaTupla :: (Eq a) => (a, t0) -> (a, t1) -> Bool  
agrupaTupla (a1, b1) (a2, b2) = a1==a2

agrupaVal :: (Eq a) => (t0, a) -> (t1, a) -> Bool  
agrupaVal (a1, b1) (a2, b2) = b1==b2

-- | funções para trabalhar com tuplas
foldByKey' g   = foldl1' (\(k1,v1) (k2,v2) -> (k1, g v1 v2))
mapByKey   g   = map (\(k,v) -> (k, g v))
sortByKey      =  sortBy ordenaTupla
sortByValue    =  sortBy ordenaTuplaVal
groupByKey     =  groupBy agrupaTupla
groupByValue   =  groupBy agrupaVal

-- | padrões de paralelismo
--combine :: ((a,c) -> (a,b) -> (a,c)) -> [(a,b)] -> [(a,c)]
combine :: Ord k 
        => (v -> v -> v) -> [(k, v)] -> [(k, v)]
combine f xs = map (foldByKey' f) $ groupByKey $ sortByKey xs

                 
mapReduceByKey  :: (NFData k, NFData v, Ord k) 
                => (a -> (k, v)) -> (v -> v -> v) -> ChunksOf [a] -> [(k, v)]
mapReduceByKey f g xs = combine g 
                     $ concat 
                     $! (map f' xs)
  where
    f' xi = combine g $ map f xi