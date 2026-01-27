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
          (tmap2_arg1 : MathExpr) 
          (tmap2_arg2 : MathExpr) 
        : MathExpr) 
      : MathExpr -> MathExpr -> MathExpr) 
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
          (tmap2_arg1 : t0) 
          (tmap2_arg2 : t0) 
        : t0) 
      : {Num t0, Num t0} t0 -> t0 -> t0)   ← 注：重複した制約
      (contract 
        (tensorMap2 
          (\tmapVar1 tmapVar0 -> 
            ((* : {Num t0} a -> a -> a)     ← 注：aという型変数
              (tmapVar1 : t0) 
              (tmapVar0 : t0) 
            : {Num t0} a)                   ← 注：不要な制約
          : {Num t0} t0 -> t0 -> a)         ← 注：不要な制約
          (t1 : Tensor t0) 
          (t2 : Tensor t0) 
        : Tensor t0)
      : [Tensor t0]) 
    : t0) 
  : Tensor t0 -> Tensor t0 -> t0)
```

tensorMap2を挿入するために作られた関数の型がおかしい。
まずこれらには、クラス制約を付加する必要はない。
また、tmap2_artNの型はTensor t0とすべきで、t0のままではない。
正しくは下記のようにあるべき。

```
def . : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\t1 t2 -> 
    ((foldl1 : (t0 -> Tensor t0 -> t0) -> [Tensor t0] -> t0) 
      (\tmap2_arg1 tmap2_arg2 -> 
        (tensorMap2 
          (+ : {Num t0} t0 -> t0 -> t0) 
          (tmap2_arg1 : t0) 
          (tmap2_arg2 : t0) 
        : t0) 
      : Tensor t0 -> Tensor t0 -> Tensor t0)   ← 注：型制約なし、テンソル型
      (contract 
        (tensorMap2 
          (\tmapVar1 tmapVar0 -> 
            ((* : {Num t0} t0 -> t0 -> t0)     ← 注：t0型
              (tmapVar1 : t0) 
              (tmapVar0 : t0) 
            : t0)                   ← 注：不要な制約なし、t0型
          : Tensor t0 -> Tensor t0 -> Tensor t0)         ← 注：不要な制約なし、Tensor t0型
          (t1 : Tensor t0) 
          (t2 : Tensor t0) 
        : Tensor t0)
      : [Tensor t0]) 
    : t0) 
  : Tensor t0 -> Tensor t0 -> t0)
```