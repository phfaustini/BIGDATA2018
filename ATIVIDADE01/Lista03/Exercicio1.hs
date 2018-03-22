module Main where

{-Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x 
for divisível por todos os números de 1 a 20.-}
divisivelN x n
    | n == 10 = True
    | mod x n /= 0 = False
    | mod x n == 0 = divisivelN x (n-1)

divisivel20 x = divisivelN x 20 -- length ([e | e <- [1..20], (mod x e) == 0 ]) == 20

main :: IO ()
main = do
    putStrLn "Informe um numero: "
    a <- getLine
    if divisivel20 (read a) then putStrLn "Numero eh divisel por todos os numeros de 1 a 20" else putStrLn "Numero NAO eh divisel por todos os numeros de 1 a 20"