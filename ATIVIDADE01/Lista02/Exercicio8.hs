module Main where

factorial n 
    | n < 2 = 1
    | otherwise = n * factorial (n-1)

coeficienteBinomial m n
    | n < 0 = 0
    | n > m = 0
    | n == 0 = 1
    | m == n = 1
    | otherwise = factorial m / factorial n * factorial (m-n)

pascalElement = coeficienteBinomial

main :: IO ()
main = do
    putStrLn "Informe um numero"
    a <- getLine
    putStrLn "Informe um numero"
    b <- getLine
    print (pascalElement (read a) (read b))