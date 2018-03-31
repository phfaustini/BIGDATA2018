{-http://folivetti.github.io/courses/Haskell/Exercicios5-}

{-Exercício 01: Resolva o problema da Zebra utilizando ADTs para representar as soluções. 
Para encontrar a resposta você deve enumerar todas as combinações até que encontre uma que atenda 
todas as restrições.-}

module Main where

data Nations = ENGLISH | SPAIN | UKRAINE | NORWAY | JAPAN deriving (Show, Enum, Eq)

data Colours = GREEN | RED | BLUE | IVORY | YELLOW deriving (Show, Enum, Eq)

data Pets = DOG | FOX | HORSE | SNAILS | ZEBRA  deriving (Show, Enum, Eq)

data Beverage = TEA | ORANGE | MILK | COFEE | WATER  deriving (Show, Enum, Eq)

data Drug = OLDGOLD | KOOLS | PARLIAMENT | LUCKYSTRIKE | CHESTERFIELDS deriving (Show, Enum, Eq)

data Row = Row Nations Colours Pets Beverage Drug deriving (Show, Eq)


nations =   [ENGLISH,   SPAIN,    UKRAINE,     NORWAY,        JAPAN]

colours =   [GREEN,     RED,      BLUE,        IVORY,         YELLOW]

pets =      [DOG,       FOX,      HORSE,       SNAILS,        ZEBRA]

beverages = [TEA,       ORANGE,   MILK,         COFEE,         WATER]

drugs =     [OLDGOLD,   KOOLS,    PARLIAMENT,  LUCKYSTRIKE,   CHESTERFIELDS]

rowPossible = [ Row n c p b d | n<-nations , c<-colours, p <-pets, b<-beverages , d<-drugs ]

allPossibilities = [ [a,b,c,d,e] | a<-rowPossible, b<-rowPossible, c<-rowPossible,d<-rowPossible,e<-rowPossible  ]

v = [Row NORWAY YELLOW FOX WATER KOOLS, Row UKRAINE BLUE HORSE TEA CHESTERFIELDS,  Row ENGLISH RED SNAILS MILK OLDGOLD, Row SPAIN IVORY DOG ORANGE LUCKYSTRIKE,    Row JAPAN GREEN ZEBRA COFEE PARLIAMENT]


exercicios5 = v `elem` allPossibilities


main::IO ()
main = print exercicios5