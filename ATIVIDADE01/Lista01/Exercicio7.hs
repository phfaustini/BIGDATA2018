module Main where

{-Exercício 07: Faa uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo-}
halfSin :: Floating a => a -> (a,a)
halfSin a = (sqrt ((1 - cos a) / 2), (-1)*sqrt ((1 - cos a) / 2))

main :: IO ()
main = do
    putStrLn "Digite um numero: "
    number <- getLine
    print (halfSin (read number))