module Main where

{-Exercício 03: Faça uma função que calcule a soma da diagonal secundária de uma matriz.-}
sumSecondaryDiagonal matrix line position sum
    | line == 1 = sum + (head matrix !! position)
    | otherwise = sumSecondaryDiagonal matrix (line - 1) (position + 1) (sum + (matrix !! (line - 1) !! position) )

sumSecondaryDiagonalMatrix matrix = sumSecondaryDiagonal matrix (length matrix) 0 0

main :: IO ()
main = print (sumSecondaryDiagonalMatrix [[1,2,3],[5,5,5],[31,1,2]])