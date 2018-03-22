module Main where

{-Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.-}
multiplicaCabecas a b = head a * head b

produtoEscalar a b
    | length a == length b && length a == 1 = multiplicaCabecas a b
    | otherwise = multiplicaCabecas a b + produtoEscalar (tail a) (tail b)

main :: IO ()
main = print (produtoEscalar [10,9,8,7,6] [1,2,3,4,5])
    