module Main where
    
somaDigitos :: Integral t => t -> t
somaDigitos x
    | resto == x = x
    | otherwise = resto + somaDigitos (div x 10)
    where resto = mod x 10

{-Exercício 06: Faça uma função que calcule a persistência aditiva de um número.-}
persistenciaAditivaCalcular number sums
    | x < 10 = sums
    | otherwise = persistenciaAditivaCalcular (somaDigitos x) (sums + 1)
    where x = abs number

persistenciaAditiva number = persistenciaAditivaCalcular number 0

main :: IO ()
main = do
    putStrLn "Informe um numero"
    a <- getLine
    putStr "A persistencia aditiva de seus digitos vale "
    print (persistenciaAditiva (read a))