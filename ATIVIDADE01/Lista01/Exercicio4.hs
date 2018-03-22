module Main where

mult3 :: (Eq x, Num x, Integral x) => x -> Bool
mult3 x
    | (==)(mod x 3) 0 = True
    | otherwise = False

mult5 :: (Eq x, Num x, Integral x) => x -> Bool
mult5 x
    | (==) (mod x 5) 0 = True
    | otherwise = False

{-Exercício 04: Faça uma função mult35 x que retorne True caso a entrada seja múltiplo de 3 e 5 e False caso contrário.-}
mult35 :: (Eq x, Num x, Integral x) => x -> Bool
mult35 x
    | mult3 x && mult5 x = True
    | otherwise = False


main :: IO ()
main = do
    putStrLn "Digite um numero: "
    number <- getLine
    if mult35 $ read number then putStrLn "Numero informado eh multiplo de 3 e 5" else putStrLn "Numero informado NAO multiplo de 3 e 5"