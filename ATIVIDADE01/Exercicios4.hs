{- http://folivetti.github.io/courses/Haskell/Exercicios4 -}

module Exercicios4
(
    generateRow,
    buildMatrix,
    idMatrix,
    sumMainDiagonal,
    sumMainDiagonalMatrix,
    sumSecondaryDiagonal,
    sumSecondaryDiagonalMatrix
)where

{-Exercício 01: Faça uma função que gere uma matriz identidade de tamanho n.-}
generateRow n pos = [if x == pos then 1 else 0 | x <- [1..n]] -- Indice começa em 1

buildMatrix n line outputList   -- Cria uma linha por vez da matriz, deslocando o 1 da diagonal principal
    | line == 1 = generateRow n line : outputList
    | otherwise = buildMatrix n (line - 1) (generateRow n line : outputList)

idMatrix n = buildMatrix n n []


{-Exercício 02: Faça uma função que calcule a soma da diagonal principal de uma matriz.-}
sumMainDiagonal matrix line sum
    | line == 1 = sum + head (head matrix)
    | otherwise = sumMainDiagonal matrix (line - 1) (sum + (matrix !! (line - 1) !! (line - 1)) )

sumMainDiagonalMatrix matrix = sumMainDiagonal matrix (length matrix) 0 

{-Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz.-}
sumSecondaryDiagonal matrix line position sum
    | line == 1 = sum + (head matrix !! position)
    | otherwise = sumSecondaryDiagonal matrix (line - 1) (position + 1) (sum + (matrix !! (line - 1) !! position) )

sumSecondaryDiagonalMatrix matrix = sumSecondaryDiagonal matrix (length matrix) 0 0