{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Language.Egison.Math.Rewrite
Licence     : MIT

This module implements rewrite rules for common mathematical functions.
-}

module Language.Egison.Math.Rewrite
  ( rewriteSymbol
  ) where

import           Control.Egison

import           Language.Egison.Math.Arith
import           Language.Egison.Math.Expr
import           Language.Egison.Math.Normalize
import {-# SOURCE #-} Language.Egison.Data (WHNFData)


rewriteSymbol :: ScalarData -> ScalarData
rewriteSymbol =
  foldl1 (\acc f -> f . acc)
    [ rewriteI
    , rewriteW
    , rewriteLog
    , rewriteSinCos
    , rewriteExp
    , rewritePower
    , rewriteSqrt
    , rewriteRt
    , rewriteRtu
    , rewriteDd
    ]

mapTerms :: (TermExpr -> TermExpr) -> ScalarData -> ScalarData
mapTerms f (Div (Plus ts1) (Plus ts2)) =
  Div (Plus (map f ts1)) (Plus (map f ts2))

mapTerms' :: (TermExpr -> ScalarData) -> ScalarData -> ScalarData
mapTerms' f (Div (Plus ts1) (Plus ts2)) =
  mathDiv (foldl mathPlus (Div (Plus []) (Plus [Term 1 []])) (map f ts1)) (foldl mathPlus (Div (Plus []) (Plus [Term 1 []])) (map f ts2))

mapPolys :: (PolyExpr -> PolyExpr) -> ScalarData -> ScalarData
mapPolys f (Div p1 p2) = Div (f p1) (f p2)

rewriteI :: ScalarData -> ScalarData
rewriteI = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (symbol #"i", $k) : $xss ->
              if even k
                then Term (a * (-1) ^ (quot k 2)) xss
                else Term (a * (-1) ^ (quot k 2)) ((Symbol "" "i" [], 1) : xss) |]
      , [mc| _ -> term |]
      ]

rewriteW :: ScalarData -> ScalarData
rewriteW = mapPolys g . mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (symbol #"w", $k & ?(>= 3)) : $xss ->
               Term a ((Symbol "" "w" [], k `mod` 3) : xss) |]
      , [mc| _ -> term |]
      ]
  g poly@(Plus ts) =
    match dfs ts (Multiset TermM)
      [ [mc| term $a ((symbol #"w", #2) : $mr) :
             term $b ((symbol #"w", #1) : #mr) : $pr ->
               g (Plus (Term (-a) mr :
                        Term (b - a) ((Symbol "" "w" [], 1) : mr) : pr)) |]
      , [mc| _ -> poly |]
      ]

rewriteLog :: ScalarData -> ScalarData
rewriteLog = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply1 #"log" _ zero, _) : _ -> Term 0 [] |]
      , [mc| (apply1 #"log" _ (singleTerm _ #1 [(symbol #"e", $n)]), _) : $xss ->
              Term (n * a) xss |]
      , [mc| _ -> term |]
      ]

makeApply :: WHNFData -> [ScalarData] -> SymbolExpr
makeApply f args =
  makeApplyExpr (SingleSymbol (QuoteFunction f)) args

rewriteExp :: ScalarData -> ScalarData
rewriteExp = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply1 #"exp" _ zero, _) : $xss ->
               f (Term a xss) |]
      , [mc| (apply1 #"exp" _ (singleTerm #1 #1 []), _) : $xss ->
               f (Term a ((Symbol "" "e" [], 1) : xss)) |]
      , [mc| (apply1 #"exp" _ (singleTerm $n #1 [(symbol #"i", #1), (symbol #"π", #1)]), _) : $xss ->
               f (Term ((-1) ^ n * a) xss) |]
      , [mc| (apply1 #"exp" $expWhnf $x, $n & ?(>= 2)) : $xss ->
               f (Term a ((makeApply expWhnf [mathScalarMult n x], 1) : xss)) |]
      , [mc| (apply1 #"exp" $expWhnf $x, #1) : (apply1 #"exp" _ $y, #1) : $xss ->
               f (Term a ((makeApply expWhnf [mathPlus x y], 1) : xss)) |]
      , [mc| _ -> term |]
      ]

rewritePower :: ScalarData -> ScalarData
rewritePower = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply1 #"^" _ (singleTerm #1 #1 []), _) : $xss -> f (Term a xss) |]
      , [mc| (apply2 #"^" $powerWhnf $x $y, $n & ?(>= 2)) : $xss ->
               f (Term a ((makeApply powerWhnf [x, mathScalarMult n y], 1) : xss)) |]
      , [mc| (apply2 #"^" $powerWhnf $x $y, #1) : (apply2 #"^" _ #x $z, #1) : $xss ->
               f (Term a ((makeApply powerWhnf [x, mathPlus y z], 1) : xss)) |]
      , [mc| _ -> term |]
      ]

rewriteSinCos :: ScalarData -> ScalarData
rewriteSinCos = mapPolys h . mapTerms (g . f)
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply1 #"sin" _ zero, _) : _ -> Term 0 [] |]
      , [mc| (apply1 #"sin" _ (singleTerm _ #1 [(symbol #"π", #1)]), _) : _ ->
               Term 0 [] |]
      , [mc| (apply1 #"sin" _ (singleTerm $n #2 [(symbol #"π", #1)]), $m) : $xss ->
              Term (a * (-1) ^ (div (abs n - 1) 2) * m) xss |]
      , [mc| _ -> term |]
      ]
  g term@(Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply1 #"cos" _ zero, _) : $xss -> Term a xss |]
      , [mc| (apply1 #"cos" _ (singleTerm _ #2 [(symbol #"π", #1)]), _) : _ ->
              Term 0 [] |]
      , [mc| (apply1 #"cos" _ (singleTerm $n #1 [(symbol #"π", #1)]), $m) : $xss ->
               Term (a * (-1) ^ (abs n * m)) xss |]
      , [mc| _ -> term |]
      ]
  h poly@(Plus ts) =
    match dfs ts (Multiset TermM)
      [ [mc| (term $a ((apply1 #"cos" $cosWhnf $x, #2) : $mr)) : (term $b ((apply1 #"sin" $sinWhnf #x, #2) : #mr)) : $pr ->
              h (Plus (Term a mr : Term (b - a) ((makeApply sinWhnf [x], 2) : mr) : pr)) |]
      , [mc| _ -> poly |]
      ]

rewriteSqrt :: ScalarData -> ScalarData
rewriteSqrt = mapTerms' f
 where
  f (Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply1 #"sqrt" $sqrtWhnf $x, ?(> 1) & $k) : $xss ->
               rewriteSqrt
                 (mathMult (SingleTerm a ((makeApply sqrtWhnf [x], k `mod` 2) : xss))
                           (mathPower x (div k 2))) |]
      , [mc| (apply1 #"sqrt" $sqrtWhnf (singleTerm $n #1 $x), #1) :
               (apply1 #"sqrt" _ (singleTerm $m #1 $y), #1) : $xss ->
             let d@(Term c z) = termsGcd [Term n x, Term m y]
                 Term n' x' = mathDivideTerm (Term n x) d
                 Term m' y' = mathDivideTerm (Term m y) d
                 in case (n' * m', Term n' x', Term m' y') of
                      (1, Term _ [], Term _ []) -> mathMult (SingleTerm c z) (SingleTerm a xss)
                      (_, _, _) -> mathMult (SingleTerm c z) (SingleTerm a ((makeApply sqrtWhnf [SingleTerm (n' * m') (x' ++ y')], 1) : xss)) |]
      , [mc| _ -> SingleTerm a xs |]
      ]

rewriteRt :: ScalarData -> ScalarData
rewriteRt = mapTerms' f
 where
  f (Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply2 #"rt" _ (singleTerm $n #1 []) $x & $rtnx, ?(>= n) & $k) : $xss ->
               mathMult (SingleTerm a ((rtnx, k `mod` n) : xss))
                        (mathPower x (div k n)) |]
      , [mc| _ -> SingleTerm a xs |]
      ]

rewriteRtu :: ScalarData -> ScalarData
rewriteRtu = mapTerms' g . mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply1 #"rtu" _ (singleTerm $n #1 []) & $rtun, ?(>= n) & $k) : $r ->
               Term a ((rtun, k `mod` n) : r) |]
      , [mc| _ -> term |]
      ]
  g (Term a xs) =
    match dfs xs (Multiset (SymbolM, Eql))
      [ [mc| (apply1 #"rtu" _ (singleTerm $n #1 []) & $rtun, ?(== n - 1)) : $mr ->
               mathMult
                 (foldl mathMinus (SingleTerm (-1) []) (map (\k -> SingleTerm 1 [(rtun, k)]) [1..(n-2)]))
                 (g (Term a mr)) |]
      , [mc| _ -> SingleTerm a xs |]
      ]

rewriteDd :: ScalarData -> ScalarData
rewriteDd (Div (Plus p1) (Plus p2)) =
  Div (Plus (rewriteDdPoly p1)) (Plus (rewriteDdPoly p2))
 where
  rewriteDdPoly poly =
    match dfs poly (Multiset TermM)
      [ [mc| term $a (($f & func $g $arg, $n) : $mr) :
               term $b ((func #g #arg, #n) : #mr) : $pr ->
                 rewriteDdPoly (Term (a + b) ((f, n) : mr) : pr) |]
      , [mc| _ -> poly |]
      ]
