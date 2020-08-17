{-# LANGUAGE QuasiQuotes #-}

module Language.Egison.Math.Rewrite
  ( rewriteSymbol
  ) where

import           Control.Egison

import           Language.Egison.Math.Arith
import           Language.Egison.Math.Expr
import           Language.Egison.Math.Normalize


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
  mathDiv (foldl1 mathPlus (map f ts1)) (foldl1 mathPlus (map f ts2))

mapPolys :: (PolyExpr -> PolyExpr) -> ScalarData -> ScalarData
mapPolys f (Div p1 p2) = Div (f p1) (f p2)

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

rewriteW :: ScalarData -> ScalarData
rewriteW = mapPolys g . mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
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
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"log" [zero], _) : _ -> Term 0 [] |]
      , [mc| (apply #"log" [singleTerm _ #1 [(symbol #"e", $n)]], _) : $xss ->
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
      , [mc| (apply #"exp" [singleTerm #1 #1 []], _) : $xss ->
               f (Term a ((Symbol "" "e" [], 1) : xss)) |]
      , [mc| (apply #"exp" [singleTerm $n #1 [(symbol #"i", #1), (symbol #"π", #1)]], _) : $xss ->
               f (Term ((-1) ^ n * a) xss) |]
      , [mc| (apply #"exp" [$x], $n & ?(>= 2)) : $xss ->
               f (Term a ((makeApply "exp" [mathScalarMult n x], 1) : xss)) |]
      , [mc| (apply #"exp" [$x], #1) : (apply #"exp" [$y], #1) : $xss ->
               f (Term a ((makeApply "exp" [mathPlus x y], 1) : xss)) |]
      , [mc| _ -> term |]
      ]

rewritePower :: ScalarData -> ScalarData
rewritePower = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"^" [singleTerm #1 #1 [], _], _) : $xss -> f (Term a xss) |]
      , [mc| (apply #"^" [$x, $y], $n & ?(>= 2)) : $xss ->
               f (Term a ((makeApply "^" [x, mathScalarMult n y], 1) : xss)) |]
      , [mc| (apply #"^" [$x, $y], #1) : (apply #"^" [#x, $z], #1) : $xss ->
               f (Term a ((makeApply "^" [x, mathPlus y z], 1) : xss)) |]
      , [mc| _ -> term |]
      ]

rewriteSinCos :: ScalarData -> ScalarData
rewriteSinCos = mapTerms' h . mapTerms (g . f)
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"sin" [zero], _) : _ -> Term 0 [] |]
      , [mc| (apply #"sin" [singleTerm _ #1 [(symbol #"π", #1)]], _) : _ ->
               Term 0 [] |]
      , [mc| (apply #"sin" [singleTerm $n #2 [(symbol #"π", #1)]], $m) : $xss ->
              Term (a * (-1) ^ (div (abs n - 1) 2) * m) xss |]
      , [mc| _ -> term |]
      ]
  g term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"cos" [zero], _) : $xss -> Term a xss |]
      , [mc| (apply #"cos" [singleTerm _ #2 [(symbol #"π", #1)]], _) : _ ->
              Term 0 [] |]
      , [mc| (apply #"cos" [singleTerm $n #1 [(symbol #"π", #1)]], $m) : $xss ->
               Term (a * (-1) ^ (abs n * m)) xss |]
      , [mc| _ -> term |]
      ]
  h term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"cos" [$x], #2) : $mr ->
               mathMult
                 (mathMinus (SingleTerm 1 []) (SingleTerm 1 [(makeApply "sin" [x], 2)]))
                 (h (Term a mr)) |]
      , [mc| _ -> Div (Plus [term]) (Plus [Term 1 []]) |]
      ]

rewriteSqrt :: ScalarData -> ScalarData
rewriteSqrt = mapTerms' f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"sqrt" [$x], ?(> 1) & $k) : $xss ->
               rewriteSqrt
                 (mathMult (SingleTerm a ((makeApply "sqrt" [x], k `mod` 2) : xss))
                           (mathPower x (div k 2))) |]
      , [mc| (apply #"sqrt" [singleTerm $n #1 $x], #1) :
               (apply #"sqrt" [singleTerm $m #1 $y], #1) : $xss ->
             let d = termsGcd [Term n x, Term m y]
                 Term n' x' = mathDivideTerm (Term n x) d
                 Term m' y' = mathDivideTerm (Term m y) d
                 sqrtxy = (if x' == [] then [] else [(makeApply "sqrt" [SingleTerm 1 x'], 1)]) ++ 
                            (if y' == [] then [] else [(makeApply "sqrt" [SingleTerm 1 y'], 1)])
              in mathMult
                   (Div (Plus [d]) (Plus [Term 1 []]))
                   (SingleTerm
                     a
                     (sqrtxy ++
                      (makeApply "sqrt" [SingleTerm (n' * m') []], 1) : xss)) |]
      , [mc| _ -> Div (Plus [term]) (Plus [Term 1 []]) |]
      ]

rewriteRt :: ScalarData -> ScalarData
rewriteRt = mapTerms' f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"rt" [singleTerm $n #1 [], $x] & $rtnx, ?(>= n) & $k) : $xss ->
               mathMult (SingleTerm a ((rtnx, k `mod` n) : xss))
                        (mathPower x (div k n)) |]
      , [mc| _ -> Div (Plus [term]) (Plus [Term 1 []]) |]
      ]

rewriteRtu :: ScalarData -> ScalarData
rewriteRtu = mapTerms f
 where
  f term@(Term a xs) =
    match dfs xs (Multiset (Pair SymbolM Eql))
      [ [mc| (apply #"rtu" [singleTerm $n #1 []] & $rtun, ?(>= n) & $k) : $r ->
               Term a ((rtun, k `mod` n) : r) |]
      , [mc| _ -> term |]
      ]

rewriteDd :: ScalarData -> ScalarData
rewriteDd (Div (Plus p1) (Plus p2)) =
  Div (Plus (rewriteDdPoly p1)) (Plus (rewriteDdPoly p2))
 where
  rewriteDdPoly poly =
    match dfs poly (Multiset TermM)
      [ [mc| term $a (($f & func $g $arg $js, $n) : $mr) :
               term $b ((func #g #arg #js, #n) : #mr) : $pr ->
                 rewriteDdPoly (Term (a + b) ((f, n) : mr) : pr) |]
      , [mc| _ -> poly |]
      ]
