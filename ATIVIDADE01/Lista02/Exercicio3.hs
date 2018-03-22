module Main where

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

main :: IO ()
main = do
    putStrLn "Informe um numero"
    a <- getLine
    putStrLn "Informe um numero"
    b <- getLine
    print (ethiopianMultiplication (read a) (read b))