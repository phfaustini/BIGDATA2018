module Main where

{-Exercício 02: Faça uma função mult3 x que retorne True caso a entrada seja múltiplo de 3 e False caso contrário.-}
mult3 :: (Eq x, Num x, Integral x) => x -> Bool
mult3 x
    | (==)(mod x 3) 0 = True
    | otherwise = False


main :: IO ()
main = do
    putStrLn "Digite um numero: "
    number <- getLine
    if mult3 $ read number then putStrLn "Numero informado eh multiplo de 3" else putStrLn "Numero informado NAO multiplo de 3"