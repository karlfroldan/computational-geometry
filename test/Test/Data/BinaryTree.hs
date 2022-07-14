module Test.Data.BinaryTree where 

import Data.BinaryTree
import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as S

bintreeInsertionTests = describe "BinaryTree Insertion Tests" $ do 
    it "Inserting using insertWithUpdate into an existing node updates the value" $ do 
        let tree = fromList [(1, 0)] :: BinTree Int Int
            tree' = insertWithUpdate id (+) 1 1 tree 
        tree' `shouldBe` fromList [(1, 1)] 
    
    it "Inserting using insertWithUpdate with a Set value." $ do 
        let tree = fromList [(0, S.fromList [3.4, 2.0]), (-100, S.fromList [1, 5])] :: BinTree Int (S.Set Double)
            tree' = insertWithUpdate id (<>) (-100) (S.fromList [-2]) tree
        tree' `shouldBe` fromList [(0, S.fromList [3.4, 2.0]), (-100, S.fromList [1, -2, 5])]
