module Main where

collatz x
    | mod x 2 == 0 = div (fromIntegral x) 2
    | otherwise = x*3 + 1


collatzLen x = computeLength x 0
    where 
        computeLength n size
            | next == 1 = size + 1
            | otherwise = computeLength next (size + 1)
            where
                next = collatz n


{-Exercício 08: Encontre o número x entre 1 e 1000000 que tem a maior sequência de Collatz. (Project Euler 14)-}
maxCollatzGen candidate max maxLen
    | candidate <= limit && candLen > maxLen = maxCollatzGen (candidate+1) candidate candLen 
    | candidate <= limit && candLen <= maxLen = maxCollatzGen (candidate+1) max maxLen
    | otherwise = max
        where 
            limit = 1000000
            candLen = collatzLen candidate

projectEuler14 = maxCollatzGen 2 1 3

main :: IO ()
main = print projectEuler14    