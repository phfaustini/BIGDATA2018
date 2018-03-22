module Main where

{-Exercício 05: Faça um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E múltiplo de 2), e False caso contrário.-}
ex5 :: (Eq x, Num x, Integral x) => x -> Bool
ex5 x
    | x < -1 || (x > 1 && (==) (mod x 2) 0) = True
    | otherwise = False


main :: IO ()
main = do
    putStrLn "Digite um numero: "
    number <- getLine
    print (ex5 (read number))