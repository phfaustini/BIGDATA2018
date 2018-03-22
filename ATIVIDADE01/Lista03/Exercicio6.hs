module Main where

{-Exercício 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.-}
collatz x
    | mod x 2 == 0 = div (fromIntegral x) 2
    | otherwise = x*3 + 1

main :: IO ()
main = do
    putStrLn "Informe um numero: "
    a <- getLine
    print (collatz (read a))