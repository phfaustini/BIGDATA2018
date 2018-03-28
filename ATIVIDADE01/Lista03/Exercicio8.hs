module Main where

import Data.List -- maximumBy
import Data.Ord -- comparing
import qualified Data.Array as Array

limit :: Int
limit = 1000000

collatz :: Integral a => a -> a
collatz x
    | mod x 2 == 0 = div (fromIntegral x) 2
    | otherwise = x*3 + 1

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- VERSÃO MEMOIZATION

memoLenCollatz :: Array.Array Int Integer
memoLenCollatz = lengths
    where
        lengths = Array.listArray (1, limit) [memoLen x | x <- [1..limit]] -- lengths :: (Enum e, Num i, Num e, Array.Ix i) => Array.Array i e
        memoLen 1 = 1
        memoLen n
            | next <= limit = 1 + lengths Array.! next -- lengths Array.! next significa o valor em lengths no indice next
            | otherwise = 1 + memoLen next -- next pode ser até 2999998 >= 1000000, quando n vale 999999. Então não se registra nada no array para evitar estouro.
            where 
                next = collatz n


collatzSequencesLengths :: [(Int, Integer)]
collatzSequencesLengths = take limit $ Array.assocs memoLenCollatz   -- retorna lista de tuplas (indice, valor)

biggestMemoized = fst $ maximumBy (comparing snd) collatzSequencesLengths  -- retorna o maior elemento da lista comparado pelo segundo elemento da array
------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- VERSÃO FORCA BRUTA

collatzLen x = computeLength x 0
    where 
        computeLength n size
            | next == 1 = size + 1
            | otherwise = computeLength next (size + 1)
            where
                next = collatz n

maxCollatzGen candidate max maxLen
    | candidate <= limit && candLen > maxLen = maxCollatzGen (candidate+1) candidate candLen 
    | candidate <= limit && candLen <= maxLen = maxCollatzGen (candidate+1) max maxLen
    | otherwise = max
        where 
            limit = 1000000
            candLen = collatzLen candidate
 
biggestBruteForce = maxCollatzGen 2 1 3
------------------------------------------------------------------------------------------------------------------------------------------------------------------------


main :: IO ()
main = print biggestMemoized