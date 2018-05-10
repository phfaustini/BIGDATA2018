{- |
Module      :  Main.hs
Description :  Entry point and IO functions.
Copyright   :  Copyright (c) 2018 Pedro Faustini
License     :  See LICENSE

Maintainer  :  pedro.faustini@ufabc.edu.br
Stability   :  stable
Portability :  non-portable (Tested only in Linux)

This module is the Main module, the entry point of the program.
All impure actions, like IO, are contained here, and only here.
-}

module Main where

import System.Environment -- getArgs
import TransactionsHandler
import FPTree -- minsup, numberChunks
import FPGrowth
import qualified Data.Map as Map
import Data.List -- intercalate
import Data.List.Split -- chunksOf n list

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile


printFPTree :: FPNode -> String -> String
printFPTree node spaces
    | not (null (fpchildren node)) = "\n"++spaces ++ fpitem node ++ " : " ++ show(fpcount node) ++ "  " ++ intercalate (spaces++"    ") [printFPTree x (spaces++"    ") | x <- fpchildren node ]
    | otherwise = "\n"++spaces ++ fpitem node ++ " : " ++show(fpcount node) ++ "  []"


main::IO ()
main = do
    args <- getArgs
    let filepath = head args
    fileContent <- readLines filepath -- "input/transactions.txt"    

    
    {-
        Step 1: Preprocessing.
        Database is read.
        TransactionsHandler functions now count items from transactions.
        Then, the headerTable is built.
        Last, infrequent items are pruned from the sorted transactions.
    -}
    let transactions = map words fileContent
    let itemsCounted = countItems transactions
    let transactionsSize = length transactions
    let threshold = round $ minsup * fromIntegral transactionsSize
    putStr "threshold "
    print threshold
    putStrLn ""    
    let itemsCountedAndPruned = applyThreshold (fromIntegral transactionsSize) itemsCounted
    
    let headerTablePruned = sortbyMostFrequent itemsCountedAndPruned
    let headerTablePrunedReversed = reverse headerTablePruned
    putStr "HeaderTableReversed pruned "
    print headerTablePrunedReversed
    putStrLn ""

    let sortedPrunedTransactions = sortTransactions transactions headerTablePrunedReversed
    putStr "Transactions Pruned "
    --print sortedPrunedTransactions
    

    {-
        Step 2: build FPTree
    -}
    let root = FPNode "null" transactionsSize []
    let fptree = buildFPTree root sortedPrunedTransactions
    putStr (printFPTree fptree " ")
    putStrLn "\n"
    

    {-
        Step 3: FPGrowth
        Conditional pattern bases are extracted from FPTree, one base for each frequent item.
        Frequent item sets are then mined.
    -}
    let headerTablePrunedfromMintoMax = headerTablePruned
    let cpbs = buildConditionalPatternBase headerTablePrunedfromMintoMax fptree
    putStr "CPBS "
    print cpbs
    putStrLn ""
    let frequentSetsItems = frequentPatternItems cpbs threshold
    putStrLn "Frequent sets of items "
    print frequentSetsItems
    putStrLn ""
    