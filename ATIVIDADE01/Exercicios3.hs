-- http://folivetti.github.io/courses/Haskell/Exercicios3
module Main where

{-Exercício 01: Crie uma função divisivel20 x que retorna verdadeiro se x 
for divisível por todos os números de 1 a 20.-}
divisivelN x n
    | n == 10 = True
    | mod x n /= 0 = False
    | mod x n == 0 = divisivelN x (n-1)

divisivel20 x = divisivelN x 20 -- length ([e | e <- [1..20], (mod x e) == 0 ]) == 20

{-Exercício 02: Crie uma função projectEuler5 que retorna o primeiro número natural que 
retorna True para a função do exercício anterior. Pense em como reduzir o custo computacional.-}

findDivisivel20 n
    | divisivel20 n = n
    | otherwise = findDivisivel20 (n+20)

projectEuler5 = findDivisivel20 20


{-Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.-}
fibonacci = 0 : 1 : fib fibonacci
    where fib (x:xs) = (x + head xs) : fib xs


{-Exercício 04: Utilizando a lista anterior, calcule a soma dos números de Fibonacci pares 
dos valores que não excedem 4.000.000. (Project Euler 2)-}
exercicio4 = sum $ filter even (takeWhile (<=4000000) fibonacci)


{-Exercício 05: Faça uma função para calcular o produto escalar entre dois vetores.-}
multiplicaCabecas a b = head a * head b

produtoEscalar a b
    | length a == length b && length a == 1 = multiplicaCabecas a b
    | otherwise = multiplicaCabecas a b + produtoEscalar (tail a) (tail b)

{-Exercício 06: Crie a função collatz x que retorna x/2, se x for par e (3x+1) se for ímpar.-}
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


{-Exercício 08: Encontre o número x entre 1 e 1000000 que tem a maior sequência de Collatz. (Project Euler 14)-}
maxCollatzGen candidate max maxLen
    | candidate <= limit && candLen > maxLen = maxCollatzGen (candidate+1) candidate candLen 
    | candidate <= limit && candLen <= maxLen = maxCollatzGen (candidate+1) max maxLen
    | otherwise = max
        where 
            limit = 1000000
            candLen = collatzLen candidate

projectEuler14 = maxCollatzGen 2 1 3

-- 837799

main::IO()
main = print projectEuler14