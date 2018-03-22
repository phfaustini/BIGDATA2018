module Main where

collatz x
    | mod x 2 == 0 = div (fromIntegral x) 2
    | otherwise = x*3 + 1

{-Exercício 07: Implemente uma função collatzLen x que retorna o tamanho da lista formada pela 
aplicação repetida de collatz sobre o valor x até que essa chegue no número 1.-}
collatzLen x = computeLength x 0
    where 
        computeLength n size
            | next == 1 = size + 1
            | otherwise = computeLength next (size + 1)
            where
                next = collatz n

main :: IO ()
main = do
    putStrLn "Informe um numero: "
    a <- getLine
    print (collatzLen (read a))