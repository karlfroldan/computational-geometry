module Test.Data.BinaryTree where

import Data.BinaryTree
import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as S
import Data.Geometry.Segment
import Data.Geometry.Point

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

{-

In segment ordering on a sweep line, we can think of the left-most point on a horizontal
sweep line to be the segment with the lowest x-intercept at point y.

-}


binaryTreePointOperations :: SpecWith ()
binaryTreePointOperations =
    describe "Operations of binary tree with points and segments" $ do
        it "Segment ordering on the sweep line" $ do
            let s1 = Segment2D (Point2D 0 (-1)) (Point2D 1 2) :: Segment2D Double
                s2 = Segment2D (Point2D 0 1) (Point2D 2 (-1))
                -- ordering at y = 0.75
                test1 = xIntercept 0.75 s1 > xIntercept 0.75 s2 `shouldBe` True
                -- ordering at y == 0.4
                test2 = xIntercept 0.4 s1 < xIntercept 0.4 s2 `shouldBe` True
            test1 <> test2

        it "The Binary Tree perfectly captures the sweep line" $ do
            let s1 = Segment2D (Point2D 0 (-1)) (Point2D 1 2) :: Segment2D Double
                s2 = Segment2D (Point2D 0 1) (Point2D 2 (-1))
                -- Insert tree at y=0.75
                tree1 = fromListWith (xIntercept 0.75) [(s1, ()), (s2, ())]

                test1 = key (maxView tree1) `shouldBe` s1
                test2 = key (minView tree1) `shouldBe` s2
            test1 <> test2

        it "The sweep line goes down y." $ do
            let s1 = Segment2D (Point2D 0 (-1)) (Point2D 1 2) :: Segment2D Double
                s2 = Segment2D (Point2D 0 1) (Point2D 2 (-1))
                -- Insert tree at y=0.75
                tree1 = fromListWith (xIntercept 0.75) [(s1, ()), (s2, ())]
                -- delete the left-most on the sweep line.
                (k, v, newTree) = deleteMin tree1
                -- and reinsert it at y=0.4
                tree2 = insertWith (xIntercept 0.4) k v newTree

                -- Their order should now be inverted.
                test1 = key (maxView tree2) `shouldBe` s2
                test2 = key (minView tree2) `shouldBe` s1
            test1 <> test2