module Test.Data.BinaryTree where 

import Data.BinaryTree
import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as S

bintreeInsertionTests :: SpecWith ()
bintreeInsertionTests = describe "BinaryTree Insertion Tests" $ do 
    it "Inserting using insertWithUpdate into an existing node updates the value" $ do 
        let tree = fromList [(1, 0)] :: BinTree Int Int
            tree' = insertWithUpdate id (+) 1 1 tree 
        tree' `shouldBe` fromList [(1, 1)] 
    
    it "Inserting using insertWithUpdate with a Set value." $ do 
        let tree = fromList [(0, S.fromList [3.4, 2.0]), (-100, S.fromList [1, 5])] :: BinTree Int (S.Set Double)
            tree' = insertWithUpdate id (<>) (-100) (S.fromList [-2]) tree
        tree' `shouldBe` fromList [(0, S.fromList [3.4, 2.0]), (-100, S.fromList [1, -2, 5])]

    it "Union with trees." $ do 
        let tree1 = fromList [(0, ()), (4, ()), (-1, ())] :: BinTree Int () 
            tree2 = fromList [(4, ()), (8, ())] :: BinTree Int () 
            tree  = tree1 `union` tree2 
        tree `shouldBe` fromList [(0, ()), (4, ()), (-1, ()), (8, ())]

    it "Intersection of trees with no common element" $ do 
        let tree1 = fromList [(0, ()), (4, ())] :: BinTree Int () 
            tree2 = fromList [(5, ())]          :: BinTree Int () 
            tree  = tree1 `intersection` tree2 
        tree `shouldBe` Leaf
    
    it "Intersection of trees with common elements" $ do 
        let tree1 = fromList [(0, ()), (4, ()), (-1, ())] :: BinTree Int ()
            tree2 = fromList [(0, ()), (-1, ()), (8, ())] :: BinTree Int ()
            tree  = tree1 `intersection` tree2 
        tree `shouldBe` fromList [(-1, ()), (0, ())]

    it "Difference of a tree with an empty tree." $ do 
        let tree1 = fromList [(0, ()), (4, ())] :: BinTree Int () 
            tree2 = Leaf 
            tree  = tree1 `difference` tree2 
        tree `shouldBe` tree1

    it "Difference of two trees" $ do 
        let tree1 = fromList [(0, ()), (4, ()), (-1, ())] :: BinTree Int ()
            tree2 = fromList [(4, ()), (-1, ())] :: BinTree Int () 
            tree  = tree1 `difference` tree2 
        tree `shouldBe` fromList [(0, ())]