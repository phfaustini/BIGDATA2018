module Main where

{-Exercício 03: Crie a lista de números de Fibonacci utilizando uma função geradora.-}
fibonacci = 0 : 1 : fib fibonacci
    where fib (x:xs) = (x + head xs) : fib xs
