-- # Compiler
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
module Compiler
    ( compile
    ) where

import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

import Types

compile :: SourceCode -> Memory
compile = postproc . map (parse rRule) . preproc

{- | 
>>> parse rRule "(once)abc=(start)ABC"
Rule {avail = Once, lhs = Pattern {fixity = Infix, pat = "abc"}, op = Prepend, rhs = "ABC"}
-}
parse :: ReadP Rule -> SourceCode -> Rule
parse r s = head [ ru | (ru, "") <- readP_to_S r s ]

rRule :: ReadP Rule
rRule = Rule <$> rAvail <*> rLhs <*> rOp <*> rRhs

rAvail, rOnce :: ReadP Availability
rAvail = option Avail rOnce
rOnce  = Once <$ string "(once)"

rLhs :: ReadP Pattern
rLhs = Pattern <$> rFixity <*> rPat

rFixity, rPrefix, rSuffix :: ReadP Fixity
rFixity = option Infix (rPrefix <++ rSuffix)
rPrefix = Prefix <$ string "(start)"
rSuffix = Suffix <$ string "(end)"

rPat :: ReadP String
rPat = munch ('=' /=)

rOp, rReturn, rPrepend, rAppend :: ReadP Operator
rOp      = string "=" *> option Subst (choice [rReturn, rPrepend, rAppend])
rReturn  = Return  <$ string "(return)"
rPrepend = Prepend <$ string "(start)"
rAppend  = Append  <$ string "(end)"

rRhs :: ReadP String
rRhs = munch (`notElem` ("()=#" :: String)) <* eof

postproc :: [Rule] -> Memory
postproc rs = listArray (1, length rs) rs

preproc :: SourceCode -> [SourceCode]
preproc = mapMaybe trim . lines

trim :: SourceCode -> Maybe SourceCode
trim sc = case break ('#' ==) sc' of
    ([],_) -> Nothing
    (xs,_) -> Just xs
    where
        sc' = filter (not . isSpace) sc
