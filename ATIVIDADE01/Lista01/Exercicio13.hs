module Main where

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

main :: IO ()
main = print (stringToInteger "0123456789")