-- # Types
--
-- 型定義を集約
--
-- ## 言語拡張と`module`宣言

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
module Types where

import Control.Monad.State
import Data.Array

type SourceCode = String
type TestCase   = (CaseNumber, TestInput, Expected)
type CaseNumber = Int
type TestInput  = String
type Expected   = String
type Judge      = Either WA AC
type WA         = String
type AC         = String

type AtoBMachine = State AtoB

type Memory = Array Addr Rule
type Addr   = Int

data AtoB
    = AtoB
    { fini :: Bool
    , mem  :: Memory
    , pc   :: Addr
    , avs  :: [Addr]
    , acc  :: String
    }
    deriving (Eq, Show)

data Rule
    = Rule
    { avail :: Availability
    , lhs   :: Pattern
    , op    :: Operator
    , rhs   :: String
    }
    deriving (Eq, Show)

data Availability
    = Once
    | Avail
    deriving (Eq, Show)

data Operator
    = Subst
    | Return
    | Prepend
    | Append
    deriving (Eq, Show)

data Pattern
    = Pattern
    { fixity :: Fixity
    , pat    :: String
    }
    deriving (Eq, Show)

data Fixity
    = Prefix
    | Suffix
    | Infix
    deriving (Eq, Show)
