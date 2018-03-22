module Main where

{-Exercício 11: Crie um concatenador de strings que concatena duas strings separadas por espaço.-}
concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ " " ++ s2

main :: IO ()
main = do
    putStrLn "Digite algo: "
    a <- getLine
    putStrLn "Digite outra coisa: "
    b <- getLine
    print (concatStrings a b)