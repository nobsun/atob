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
import System.FilePath
import System.Environment
import List.Shuffle
import Text.Printf

import AtoB

main :: IO ()
main 
    = do
    { prog <- getProgName
    ; args <- getArgs
    ; case args of
        f:str:_ -> output str . run str =<< readFile f
        _       -> usage prog
    }

output :: String -> String -> IO ()
output s t = putStr $ unlines [inp, out]
    where
        inp = printf "Input:  %s" s
        out = printf "Output: %s" t

usage :: String -> IO ()
usage prog = printf "%s %s %s" prog ("<rule-file>" :: String) ("<input-string>" :: String)
