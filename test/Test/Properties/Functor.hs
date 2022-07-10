{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant id" #-}

module Test.Properties.Functor where

testFunctorIdentity :: (Functor f, Eq a, Eq (f a)) => f a -> Bool 
testFunctorIdentity xs = fmap id xs == id xs 


testFunctorComposition :: (Eq (f b), Functor f) 
    => (c -> b) -> (a -> c) -> f a -> Bool
testFunctorComposition f g xs = fmap (f . g) xs == (fmap f . fmap g) xs