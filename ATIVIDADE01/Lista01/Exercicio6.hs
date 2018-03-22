module Main where

{-Exercício 06: Faça uma função que recebe um tipo Integer e retorna ele dividido por 2:-}
div2d :: Integer -> Double
div2d x = fromIntegral x / 2

main :: IO ()
main = do
    putStrLn "Digite um numero: "
    number <- getLine
    print (div2d (read number))