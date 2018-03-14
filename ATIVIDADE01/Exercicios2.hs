-- http://folivetti.github.io/courses/Haskell/Exercicios2

module Exercicios2
(
    leiCossenos,
    ehTriangulo,
    tipoTriangulo,
    halvesNumber,
    doublesNumber,
    sumRows,
    ethiopianMultiplication,
    encontraDivisores,
    ehPrimo,
    somaDigitos,
    persistenciaAditiva,
    persistenciaAditivaCalcular,
    factorial,
    coeficienteBinomial,
    pascalElement
)where

{- Exercício 01: Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo. -}
leiCossenos :: Fractional a => a -> a -> a -> a
leiCossenos a b c = (b*b + c*c - a*a) / (2*b*c) -- Descobre o cosseno do angulo

ehTriangulo :: (Ord a, Fractional a) => a -> a -> a -> Bool
ehTriangulo a b c
    | leiCossenos a b c >= 1 = False
    | leiCossenos b a c >= 1 = False
    | leiCossenos c a b >= 1 = False
    | otherwise = True

{-Exercício 02: Crie uma função tipoTriangulo que determina o tipo do triângulo formado pelos três lados x, y, z.-}
tipoTriangulo :: Eq a => a -> a -> a -> String
tipoTriangulo a b c
    | a /= b && a /= c && b /= c = "Escaleno"
    | a == b && a == c && b == c = "Equilatero"
    | (a == b && a /= c) || (a == c && a /= b) || (b == c && b /= a) = "Isoceles"
    

{-Exercício 03: Implemente uma função que faz a multiplicação etíope entre dois números.-}
halvesNumber x halvesList
    | x == 1 = x : halvesList
    | otherwise = halvesNumber divided (x : halvesList)
    where divided = truncate (fromIntegral x / 2)

doublesNumber x halvesList doublesList
    | length halvesList == length doublesList = doublesList
    | otherwise = doublesNumber doubled halvesList (x : doublesList)
    where doubled = x*2

sumRows halvesList doublesList = 
    sum [snd x | x <- combined, mod (fst x) 2 /= 0]
    where combined = zip halvesList doublesList

ethiopianMultiplication x y
    | x == 0 = 0
    | y == 0 = 0
    | otherwise = sumRows halves (doublesNumber y halves [])
    where halves = halvesNumber x []



{-Exercício 04: Faça uma função que determine se um número é primo.-}
encontraDivisores :: Integral t => t -> [t]
encontraDivisores x = [d | d <- [1,2..x], mod x d == 0]

ehPrimo :: Integral a => a -> Bool
ehPrimo x
    | x == 1 = False
    | x == 2 = True
    | mod x 2 == 0 = False
    | length (encontraDivisores x) > 2 = False
    | otherwise = True

{-Exercício 05: Faça uma função que calcule a soma dos dígitos de um número.-}
somaDigitos :: Integral t => t -> t
somaDigitos x
    | resto == x = x
    | otherwise = resto + somaDigitos (div x 10)
    where resto = mod x 10


{-Exercício 06: Faça uma função que calcule a persistência aditiva de um número.-}
persistenciaAditivaCalcular number sums
    | x < 10 = sums
    | otherwise = persistenciaAditivaCalcular (somaDigitos x) (sums + 1)
    where x = abs number

persistenciaAditiva number = persistenciaAditivaCalcular number 0
    


{-Exercício 07: Faça uma função que calcule o coeficiente binomial de (m,n).-}
factorial n 
    | n < 2 = 1
    | otherwise = n * factorial (n-1)

coeficienteBinomial m n
    | n < 0 = 0
    | n > m = 0
    | n == 0 = 1
    | m == n = 1
    | otherwise = factorial m / factorial n * factorial (m-n)


{-Exercício 08: Faça uma função que calcule o elemento (i,j) do triângulo de pascal.-}
pascalElement = coeficienteBinomial