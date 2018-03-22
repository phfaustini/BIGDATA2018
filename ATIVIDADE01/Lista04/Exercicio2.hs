module Main where

{-Exercício 02: Faça uma função que calcule a soma da diagonal principal de uma matriz.-}
sumMainDiagonal matrix line sum
    | line == 1 = sum + head (head matrix)
    | otherwise = sumMainDiagonal matrix (line - 1) (sum + (matrix !! (line - 1) !! (line - 1)) )

sumMainDiagonalMatrix matrix = sumMainDiagonal matrix (length matrix) 0

main :: IO ()
main = print (sumMainDiagonalMatrix [[1,2,3],[5,5,5],[31,1,2]])