module Main where

{-Exercício 03: Faça uma função mult5 x que retorne True caso a entrada seja múltiplo de 5 e False caso contrário.-}
mult5 :: (Eq x, Num x, Integral x) => x -> Bool
mult5 x
    | (==) (mod x 5) 0 = True
    | otherwise = False

main :: IO ()
main = do
    putStrLn "Digite um numero: "
    number <- getLine
    if mult5 $ read number then putStrLn "Numero informado eh multiplo de 5" else putStrLn "Numero informado NAO multiplo de 5"