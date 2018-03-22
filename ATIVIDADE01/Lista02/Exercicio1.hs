module Main where

{- Exercício 01: Crie uma função ehTriangulo que determina se três lados x, y, z podem formar um triângulo. -}
leiCossenos :: Fractional a => a -> a -> a -> a
leiCossenos a b c = (b*b + c*c - a*a) / (2*b*c) -- Descobre o cosseno do angulo

ehTriangulo :: (Ord a, Fractional a) => a -> a -> a -> Bool
ehTriangulo a b c
    | a <= 0 || b <= 0 || c <= 0 = False
    | leiCossenos a b c >= 1 = False
    | leiCossenos b a c >= 1 = False
    | leiCossenos c a b >= 1 = False
    | otherwise = True


main :: IO ()
main = do
    putStrLn "Informe o tamanho de um lado do triangulo: "
    a <- getLine
    putStrLn "Informe o tamanho do outro lado do triangulo: "
    b <- getLine
    putStrLn "Informe o tamanho de outro lado do triangulo: "
    c <- getLine
    if ehTriangulo (read a) (read b) (read c) then putStrLn "Os lado formam um triangulo" else putStrLn "Os lado NAO formam um triangulo"