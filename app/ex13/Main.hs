{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Monad
import Data.Bool
import Data.Either
import Data.List
import System.FilePath
import System.Environment
import List.Shuffle

import AtoB

solve :: String -> String
solve = map head . group where


testCaseIns :: [String]
testCaseIns = filter phi $ flip replicateM "abc" =<< [1 .. 7]
    where
        phi = const True

testCases :: [(String, String)]
testCases = map ((,) <*> solve) testCaseIns

main :: IO ()
main = do
    { prog  <- readFile . ("prog" </>) =<< getProgName
    ; cases <- shuffleIO testCases
    ; putStr $ unlines $ phi $ map (game prog) $ zipWith psi [1..] cases
    }
    where
        phi rs = case break isLeft rs of
            (xs,ys) -> map (either id id) (xs ++ bool [head ys] ys (null ys))
        psi i (x,y) = (i,x,y)

