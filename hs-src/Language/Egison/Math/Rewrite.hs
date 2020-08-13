{-# LANGUAGE QuasiQuotes #-}

module Language.Egison.Math.Rewrite
  ( rewriteSymbol
  ) where

import           Prelude                   hiding (div)
import           Control.Egison

import           Language.Egison.Math.Arith
import           Language.Egison.Math.Expr


rewriteSymbol :: ScalarData -> ScalarData
rewriteSymbol = rewriteExp . rewriteSin . rewriteLog . rewriteI

mapTerms :: (TermExpr -> TermExpr) -> ScalarData -> ScalarData
mapTerms f (Div (Plus ts1) (Plus ts2)) =
  Div (Plus (map f ts1)) (Plus (map f ts2))

rewriteI :: ScalarData -> ScalarData
rewriteI = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (symbol #"i", $k) : $xss ->
              if even k
                then Term (a * (-1) ^ (quot k 2)) xss
                else Term (a * (-1) ^ (quot k 2)) ((Symbol "" "i" [], 1) : xss) |]
      , [mc| _ -> term |]
      ]

rewriteLog :: ScalarData -> ScalarData
rewriteLog = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"log" [zero], _) : _ -> Term 0 [] |]
      , [mc| (apply #"log" [singleTerm _ [(symbol #"e", $n)]], _) : $xss ->
              Term (n * a) xss |]
      , [mc| _ -> term |]
      ]

makeApply :: String -> [ScalarData] -> SymbolExpr
makeApply f args =
  Apply (SingleSymbol (Symbol "" f [])) args

rewriteExp :: ScalarData -> ScalarData
rewriteExp = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"exp" [zero], _) : $xss ->
               f (Term a xss) |]
      , [mc| (apply #"exp" [singleTerm #1 []], _) : $xss ->
               f (Term a ((Symbol "" "e" [], 1) : xss)) |]
      , [mc| (apply #"exp" [singleTerm $n [(symbol #"i", #1), (symbol #"π", #1)]], _) : $xss ->
               f (Term ((-1) ^ n * a) xss) |]
      , [mc| (apply #"exp" [$x], $n & ?(>= 2)) : $xss ->
               f (Term a ((makeApply "exp" [mathScalarMult n x], 1) : xss)) |]
      , [mc| (apply #"exp" [$x], #1) : (apply #"exp" [$y], #1) : $xss ->
               f (Term a ((makeApply "exp" [mathPlus x y], 1) : xss)) |]
      , [mc| _ -> term |]
      ]

rewriteSin :: ScalarData -> ScalarData
rewriteSin = mapTerms f
 where
  f term@(Term _ xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"sin" [zero], _) : _ -> Term 0 [] |]
      , [mc| (apply #"sin" [singleTerm _ [(symbol #"π", #1)]], _) : _ -> Term 0 [] |]
      -- TODO
      -- , [mc| (apply #"sin" [div [term _ [(symbol #"π", #1)]] [term #2 _]], _) : $xss ->
      --         Term a xss |]
      , [mc| _ -> term |]
      ]
