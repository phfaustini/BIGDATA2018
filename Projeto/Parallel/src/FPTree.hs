{- |
Module      :  FPTree.hs
Description :  Representation of FPTree
Copyright   :  Copyright (c) 2018 Pedro Faustini
License     :  See LICENSE

Maintainer  :  pedro.faustini@ufabc.edu.br
Stability   :  stable
Portability :  non-portable (Tested only in Linux)

This module contains the definition of FPTree.
It also contains functions alike to FPTree.
-}


module FPTree
(
    numberChunks,
    buildFPTree,
    minsup,
    FPNode (FPNode),
    fpitem,
    fpcount,
    fpchildren
)
 where

numberChunks = 2 :: Int

minsup = 0.82 -- An item has to appear in at least xx% of all transactions

data FPNode = FPNode { fpitem :: ! String, fpcount :: ! Int, fpchildren :: ! [FPNode]} deriving (Show, Eq)

-- | PRIVATE
-- | It is NOT recursive on children!
hasChild :: String -> [FPNode] -> Bool
hasChild key children
    | null children = False
    | fpitem (head children) == key = True
    | otherwise = hasChild key (tail children)


-- | PRIVATE
createBranch :: [String] -> FPNode
createBranch transaction
    | length transaction == 1 = FPNode (head transaction) 1 []
    | otherwise = FPNode (head transaction) 1 [createBranch (tail transaction)]


-- | PRIVATE
insertTransaction :: [String] -> FPNode -> FPNode
insertTransaction transaction root
    | transactionLength == 1 && incrementFPCountSomeChild = FPNode (fpitem root)
                                                        (fpcount root)
                                                        (head [FPNode (fpitem x) (fpcount x + 1) (fpchildren x) | x <- fpchildren root, fpitem x == head transaction] : otherChildren)
    | transactionLength > 1 && incrementFPCountSomeChild = FPNode (fpitem root)
                                                        (fpcount root)
                                                        (head [ insertTransaction (tail transaction) (FPNode (fpitem x) (fpcount x + 1) (fpchildren x)) | x <- fpchildren root, fpitem x == head transaction] : otherChildren)
    | otherwise = FPNode (fpitem root)
                         (fpcount root)
                         (createBranch transaction : fpchildren root)
    where 
        transactionLength = length transaction
        incrementFPCountSomeChild = hasChild (head transaction) (fpchildren root)
        otherChildren = [x | x <- fpchildren root, fpitem x /= head transaction] 
    

buildFPTree :: FPNode -> [[String]] -> FPNode
buildFPTree node chunkOfTransactions
    | null chunkOfTransactions = node
    | null (head chunkOfTransactions) = buildFPTree node (tail chunkOfTransactions)
    | otherwise = buildFPTree (insertTransaction (head chunkOfTransactions) node) (tail chunkOfTransactions)
