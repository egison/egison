# 単相的なドット積

現在の--dump-typedの結果
```
def .' : Tensor MathExpr -> Tensor MathExpr -> Tensor MathExpr := 
  (\t1 t2 -> 
    ((foldl1 : (MathExpr -> Tensor MathExpr -> MathExpr) -> [Tensor MathExpr] -> MathExpr) 
      (+' : MathExpr -> MathExpr -> MathExpr) 
      (contract 
        ((*' : MathExpr -> MathExpr -> MathExpr) 
          (t1 : MathExpr) 
          (t2 : MathExpr) 
        : MathExpr) 
      : [Tensor MathExpr]) 
    : MathExpr) 
  : MathExpr -> MathExpr -> MathExpr)
```
現在の--dump-tiの結果
```
def .' : Tensor MathExpr -> Tensor MathExpr -> Tensor MathExpr := 
  (\t1 t2 -> 
    ((foldl1 : (MathExpr -> Tensor MathExpr -> MathExpr) -> [Tensor MathExpr] -> MathExpr) 
      (\tmap2_arg1 tmap2_arg2 -> 
        (tensorMap2 
          (+' : MathExpr -> MathExpr -> MathExpr) 
          (tmap2_arg1 : Tensor MathExpr) 
          (tmap2_arg2 : Tensor MathExpr) 
        : Tensor MathExpr) 
      : Tensor MathExpr -> Tensor MathExpr -> Tensor MathExpr) 
      (contract 
        ((*' : MathExpr -> MathExpr -> MathExpr) 
          (t1 : MathExpr) 
          (t2 : MathExpr) 
        : MathExpr) 
      : [Tensor MathExpr]) 
    : MathExpr) 
  : MathExpr -> MathExpr -> MathExpr)
```

# 多相的なドット積

現在の--dump-typedの結果
```
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\t1 t2 -> 
    ((foldl1 : (t0 -> Tensor t0 -> t0) -> [Tensor t0] -> t0) 
      (+ : {Num t0} t0 -> t0 -> t0) 
      (contract 
        ((* : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0) 
          (t1 : Tensor t0) 
          (t2 : Tensor t0) 
        : Tensor t0)
      : [Tensor t0]) 
    : t0) 
  : Tensor t0 -> Tensor t0 -> t0)

```

上記の結果は正しい。
クラス制約が必要な（辞書を渡すことが必要な）関数にのみクラス制約が付加されている。

現在の--dump-tiの結果
```
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\t1 t2 -> 
    ((foldl1 : (t0 -> Tensor t0 -> t0) -> [Tensor t0] -> t0) 
      (\tmap2_arg1 tmap2_arg2 -> 
        (tensorMap2 
          (+ : {Num t0} t0 -> t0 -> t0) 
          (tmap2_arg1 : Tensor t0)
          (tmap2_arg2 : Tensor t0)
        : Tensor t0) 
      : Tensor t0 -> Tensor t0 -> Tensor t0) 
      (contract 
        (tensorMap2 
          (\tmapVar1 tmapVar0 -> 
            ((* : {Num t0} t0 -> t0 -> t0) 
              (tmapVar1 : t0) 
              (tmapVar0 : t0) 
            : t0) 
          : t0 -> t0 -> t0) 
          (t1 : Tensor t0) 
          (t2 : Tensor t0) 
        : Tensor t0) 
      : [Tensor t0]) 
    : t0) 
  : Tensor t0 -> Tensor t0 -> t0)
```

上記の結果も正しい。

--dump-tcの結果
```
[283] def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\dict_Num t1 t2 -> 
    ((foldl1 : (t0 -> Tensor t0 -> t0) -> [Tensor t0] -> t0) 
      (\tmap2_arg1 tmap2_arg2 -> 
        (tensorMap2 
          (\etaVar1 etaVar2 -> 
            (((dict_Num : Hash String _)_("plus" : String) 
              : {Num t0} t0 -> t0 -> t0) 
              (etaVar1 : t0) 
              (etaVar2 : t0) 
            : t0) 
          : {Num t0} t0 -> t0 -> t0) 
          (tmap2_arg1 : Tensor t0) 
          (tmap2_arg2 : Tensor t0) 
        : Tensor t0) 
      : Tensor t0 -> Tensor t0 -> Tensor t0) 
      (contract 
        (tensorMap2 
          (\tmapVar1 tmapVar0 -> 
            (((dict_Num : Hash String _)_("times" : String) 
              : {Num t0} t0 -> t0 -> t0) 
              (tmapVar1 : t0) 
              (tmapVar0 : t0) 
            : t0) 
          : t0 -> t0 -> t0) 
          (t1 : Tensor t0) 
          (t2 : Tensor t0) 
        : Tensor t0) 
      : [Tensor t0]) 
    : t0) 
  : Tensor t0 -> Tensor t0 -> t0)
```