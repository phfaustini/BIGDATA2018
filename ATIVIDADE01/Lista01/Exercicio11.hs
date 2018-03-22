module Main where

bissextos = reverse [2016, 2012..1]

{-Exercício 10: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.-}
tupleBissextos :: ([Integer], [Integer])
tupleBissextos = (take (quot (length bissextos) 2) bissextos, drop (quot (length bissextos)  2) bissextos)

main :: IO ()
main = print tupleBissextos