module Main where

{-Exercício 05: Faça uma função que calcule a soma dos dígitos de um número.-}
somaDigitos :: Integral t => t -> t
somaDigitos x
    | resto == x = x
    | otherwise = resto + somaDigitos (div x 10)
    where resto = mod x 10

main :: IO ()
main = do
    putStrLn "Informe um numero"
    a <- getLine
    putStr "A soma de seus digitos vale "
    print (somaDigitos (read a))