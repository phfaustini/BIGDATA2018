-- http://folivetti.github.io/courses/Haskell/Exercicios1

{-
Exercício 01: Execute as seguintes operações utilizando o menor número de parênteses:
2⋅3+5
2+2⋅3+1
3**4+5⋅2**5+1
-}
module Exercicios1
(
    mult3,
    mult5,
    mult35,
    ex5,
    div2d,
    halfSin,
    bissextos,
    firstTenBissextos,
    lastTenBissextos,
    tupleBissextos,
    concatStrings,
    charToInteger,
    stringToInteger
)where

a1=2*3+5
b1=2+2*3+1
c1=3**4+5*2**5+1

{-Exercício 02: Faça uma função mult3 x que retorne True caso a entrada seja múltiplo de 3 e False caso contrário.-}
mult3 :: (Eq x, Num x, Integral x) => x -> Bool
mult3 x
    | (==)(mod x 3) 0 = True
    | otherwise = False


{-Exercício 03: Faça uma função mult5 x que retorne True caso a entrada seja múltiplo de 5 e False caso contrário.-}
mult5 :: (Eq x, Num x, Integral x) => x -> Bool
mult5 x
    | (==) (mod x 5) 0 = True
    | otherwise = False


{-Exercício 04: Faça uma função mult35 x que retorne True caso a entrada seja múltiplo de 3 e 5 e False caso contrário.-}
mult35 :: (Eq x, Num x, Integral x) => x -> Bool
mult35 x
    | mult3 x && mult5 x = True
    | otherwise = False


{-Exercício 05: Faça um programa que retorne True caso a entrada seja menor que -1 ou (maior que 1 E múltiplo de 2), e False caso contrário.-}
ex5 :: (Eq x, Num x, Integral x) => x -> Bool
ex5 x
    | x < -1 || (x > 1 && (==) (mod x 2) 0) = True
    | otherwise = False

{-Exercício 06: Faça uma função que recebe um tipo Integer e retorna ele dividido por 2:-}
div2d :: Integer -> Double
div2d x = fromIntegral x / 2

{-Exercício 07: Faa uma função que receba um ângulo a e retorne uma tupla contendo o seno da metade desse ângulo-}
halfSin :: Floating a => a -> (a,a)
halfSin a = (sqrt ((1 - cos a) / 2), (-1)*sqrt ((1 - cos a) / 2))

{-Exercício 08: Crie uma lista de anos bissextos desde o ano 1 até o atual.-}
bissextos = reverse [2016, 2012..1]

{-Exercício 09: Encontre os 10 primeiros anos bissextos.-}
firstTenBissextos :: [Integer]
firstTenBissextos = take 10 bissextos

{-Exercício 09: Encontre os 10 últimos anos bissextos (dica: use a função length para determinar o tamanho da lista).-}
lastTenBissextos :: [Integer]
lastTenBissextos = drop (length bissextos - 10) bissextos

{-Exercício 10: Crie uma tupla em que o primeiro elemento tem metade dos anos bissextos e o segundo elemento a outra metade.-}
tupleBissextos :: ([Integer], [Integer])
tupleBissextos = (take (quot (length bissextos) 2) bissextos, drop (quot (length bissextos)  2) bissextos)

{-Exercício 11: Crie um concatenador de strings que concatena duas strings separadas por espaço.-}
concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ " " ++ s2

{-Exercício 12: Dada a string “0123456789”, crie uma lista com os dígitos em formato Integer-}
charToInteger :: Char -> Integer
charToInteger c 
    | c == '0' = 0
    | c == '1' = 1
    | c == '2' = 2
    | c == '3' = 3
    | c == '4' = 4
    | c == '5' = 5
    | c == '6' = 6
    | c == '7' = 7
    | c == '8' = 8
    | c == '9' = 9
    | otherwise = -1

stringToInteger :: String -> [Integer]
stringToInteger s = [charToInteger i | i <- s ]
