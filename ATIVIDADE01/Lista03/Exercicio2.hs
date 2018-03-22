module Main where

divisivelN x n
    | n == 10 = True
    | mod x n /= 0 = False
    | mod x n == 0 = divisivelN x (n-1)

divisivel20 x = divisivelN x 20 -- length ([e | e <- [1..20], (mod x e) == 0 ]) == 20

{-Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que 
retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.-}
primesUnder20 = [2,3,5,7,11,13,17,19]

toBeIncluded x
    | x^2 < 20 = toBeIncluded (x^2)
    | otherwise = x

projectEuler5 = product [toBeIncluded x | x<- primesUnder20]
main :: IO ()
main = print projectEuler5
    