module Main where

{-Exercício 04: Faça uma função que determine se um número é primo.-}
encontraDivisores :: Integral t => t -> [t]
encontraDivisores x = [d | d <- [1,2..x], mod x d == 0]

ehPrimo :: Integral a => a -> Bool
ehPrimo x
    | x == 1 = False
    | x == 2 = True
    | mod x 2 == 0 = False
    | length (encontraDivisores x) > 2 = False
    | otherwise = True

main :: IO ()
main = do
    putStrLn "Informe um numero"
    a <- getLine
    if ehPrimo (read a) then print "Numero eh primo" else print "Numero NAO eh primo"