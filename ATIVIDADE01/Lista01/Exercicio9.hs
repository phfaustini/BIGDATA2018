module Main where

bissextos = reverse [2016, 2012..1]
{-Exercício 09: Encontre os 10 primeiros anos bissextos.-}
firstTenBissextos :: [Integer]
firstTenBissextos = take 10 bissextos

main :: IO ()
main = print firstTenBissextos