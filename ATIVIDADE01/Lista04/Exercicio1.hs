module Main where

{-Exercício 01: Faça uma função que gere uma matriz identidade de tamanho n.-}
generateRow n pos = [if x == pos then 1 else 0 | x <- [1..n]] -- Indice começa em 1

buildMatrix n line outputList   -- Cria uma linha por vez da matriz, deslocando o 1 da diagonal principal
    | line == 1 = generateRow n line : outputList
    | otherwise = buildMatrix n (line - 1) (generateRow n line : outputList)

idMatrix n = buildMatrix n n []

main :: IO ()
main = do
    putStrLn "Informe um numero"
    a <- getLine
    print (idMatrix (read a))