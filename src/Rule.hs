-- # Rule
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
module Rule where
--
-- ## import リスト
--
import Control.Monad
import Data.Bool
import Data.List
import Types
--
--
match :: Pattern -> String -> Maybe (String, String, String)
match pattern ostr = case pattern.fixity of
    Prefix
        | pattern.pat `isPrefixOf` ostr 
            -> Just ("", pattern.pat, drop (length pattern.pat) ostr)
    Suffix
        | pattern.pat `isSuffixOf` ostr 
            -> Just (reverse (drop (length pattern.pat) (reverse ostr)), pattern.pat, "")
    Infix   -> case findIndex (pattern.pat `isPrefixOf`) (tails ostr) of
        Nothing -> Nothing
        Just i  -> Just (take i ostr, pattern.pat, drop (i + length pattern.pat) ostr)
    _       -> Nothing
    
subst :: (String, String, String) -> String -> String
subst (xs,_,ys) str = concat [xs,str,ys]

showRule :: Rule -> String
showRule rule = case rule.avail of
    Once -> "(once)" ++ showRestOfRule rule
    _    -> case rule.lhs.fixity of
        Prefix -> "(start)" ++ showRestOfRule rule 
        Suffix -> "(end)"   ++ showRestOfRule rule
        _      -> showRestOfRule rule
    where
        showRestOfRule r = r.lhs.pat ++ "=" ++ showRhs r
        showRhs r = case r.op of
            Return  -> "(return)" ++ r.rhs
            Prepend -> "(start)"  ++ r.rhs
            Append  -> "(end)"    ++ r.rhs
            _       -> ""         ++ r.rhs

