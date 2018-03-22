module Main where

bissextos = reverse [2016, 2012..1]

{-Exercício 09: Encontre os 10 últimos anos bissextos (dica: use a função length para determinar o tamanho da lista).-}
lastTenBissextos :: [Integer]
lastTenBissextos = drop (length bissextos - 10) bissextos

main :: IO ()
main = print lastTenBissextos