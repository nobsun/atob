-- # AtoB (A One-Instruction)
-- 
-- ## 言語拡張と`module`宣言
--
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
module AtoB
    ( game
    , run
    ) where

--
-- ## import リスト
-- 標準ライブラリ
--
import Control.Monad.State
import Data.Array
import Data.Bool
import Data.List
import Text.Printf

import Types
import Compiler
import Rule

import Debug.Trace qualified as Debug

debug :: Bool
debug = () == ()

trace :: String -> (a -> a)
trace msg x
    | debug     = Debug.trace msg x
    | otherwise = x

-- tracing :: Show a => a -> a
-- tracing = trace . show <*> id
--
-- ## 全体概要
--
-- `game` を提供する
--
game :: SourceCode  -- ^ 問題を解く A=B 言語で書かれたソースコード
     -> TestCase    -- ^ テストケース
     -> Judge       -- ^ 判定

--
-- ### `game` 実装概要
--
game sc tc = respmsg tc res
    where
        res = eval atobMachine (initAtoB tc (compile sc))
--
-- ### マシン駆動
--
run :: String -> SourceCode -> String
run s sc = eval atobMachine (initAtoB (0,s,"") (compile sc))
--
-- ### respmsg の実装
--
respmsg :: TestCase -> String -> Judge
respmsg (i,prob,ans) sol = bool (Left wa) (Right ac) (ans == sol)
    where
        wa = header ++ bad
        ac = header ++ good 
        header = unlines [num,inp,out,you,hr]
        bad  = "Judge:                  WA\n"
        good = "Judge:                  AC\n"
        hr   = "--------------------------"
        num  = "Case Number:          %04d" `printf` i
        inp  = "Input String: %12s"         `printf` prob
        out  = "Expected:     %12s"         `printf` ans
        you  = "Your Output:  %12s"         `printf` sol

--
-- ### `eval` 実装
--
eval :: AtoBMachine String -> AtoB -> String
eval = evalState
--
-- ### `atobMachine`
--
atobMachine :: AtoBMachine String
atobMachine = do
    { ab <- get
    ; if ab.fini then return ab.acc
      else do 
        { put (execute (decode (fetch ab)) ab)
        ; atobMachine
        }
    }

fetch :: AtoB -> Rule
fetch ab = ab.mem ! ab.pc

decode :: Rule -> (AtoB -> AtoB)
decode rule ab = case match rule.lhs ab.acc of
    Nothing -> case next ab.avs ab.pc of
        Nothing -> ab { fini = True }
        Just j  -> decode (ab.mem ! j) (ab { pc = j })
    Just (xs,_,zs)
            -> trace ("    " ++ showRule rule) $ case rule.op of
                Subst   -> ab { acc = xs ++ rule.rhs ++ zs  }
                Return  -> ab { fini = True, acc = rule.rhs }
                Prepend -> ab { acc = rule.rhs ++ xs ++ zs  }
                Append  -> ab { acc = xs ++ zs ++ rule.rhs  }

execute :: (AtoB -> AtoB) -> (AtoB -> AtoB)
execute cmd ab = trace ab.acc $ if ab'.fini then ab'
    else case ava of
        Once -> case avs' of
            []   -> ab' { fini = True }
            _    -> ab' { pc = head avs', avs = avs' }
        _    -> ab' { pc = head ab'.avs }
    where
        ab'  = cmd ab
        ava  = (ab'.mem ! ab'.pc).avail
        avs' = delete ab'.pc ab'.avs

next :: [Addr] -> Addr -> Maybe Addr
next is a = case dropWhile (a >=) is of
    []  -> Nothing
    b:_ -> Just b

--
-- ### `initAtoB`
--
initAtoB :: TestCase -> Memory -> AtoB
initAtoB (_,inp,_) memory
    = AtoB { fini = False
           , mem  = memory
           , pc   = 1
           , avs  = indices memory
           , acc  = inp
           }
