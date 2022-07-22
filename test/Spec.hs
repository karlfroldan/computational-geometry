module Main where 

import Test.Hspec

import Test.Data.BinaryTree

main :: IO ()
main = hspec $ do 
    bintreeInsertionTests
    binaryTreePointOperations