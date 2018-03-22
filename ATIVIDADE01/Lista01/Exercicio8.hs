module Main where

{-Exercício 08: Crie uma lista de anos bissextos desde o ano 1 até o atual.-}
bissextos = reverse [2016, 2012..1]

main :: IO ()
main = print bissextos