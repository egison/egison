Wedgeがスカラー関数に適用された場合

```
A + C -- tensorMap2 (+) A C -- 添字を補完しない
A !+ C -- A_d1 + A_d2 -- tensorMap2 (+) A_d1 C_d2 -- 添字を補完して関数適用がされる
```

Wedgeがテンソル関数に適用された場合

```
A . C -- A . C -- 添字を補完しない
A !. C -- A_d1 . C_d2 -- 添字を補完して関数適用される
```

# 実装の流れ

```
def normalPlus {Num a} (X : Tensor a) (Y : Tensor a) : Tensor a := X + Y
def wedgePlus {Num a} (X : Tensor a) (Y : Tensor a) : Tensor a := X !+ Y
```

WedgeApplyについてもTensorMapを挿入する。

下記はcabal run egison -- --dump-ti -t mini-test/190-wedge-test.egiの結果である。

```
def normalPlus : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\X Y -> 
    (tensorMap2 
      (\tmapVar1 tmapVar0 -> 
        ((+ : {Num t0} t0 -> t0 -> t0) 
          (tmapVar1 : t0) 
          (tmapVar0 : t0) 
          : t0) 
        : t0 -> t0 -> t0) 
      (X : Tensor t0) 
      (Y : Tensor t0) 
      : Tensor t0) 
    : Tensor t0 -> Tensor t0 -> Tensor t0)
```

```
def wedgePlus : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\X Y -> 
    (! (+ : {Num t0} t0 -> t0 -> t0) 
      (X : Tensor t0) 
      (Y : Tensor t0) 
      : Tensor t0) 
    : Tensor t0 -> Tensor t0 -> Tensor t0)
```

現在、上記のようにWedgeApplyにはTensorMapが挿入されていない。

下記のように挿入するようにする。

```
def wedgePlus : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\X Y -> 
    (tensorMap2Wedge
      (\tmapVar1 tmapVar0 -> 
        ((+ : {Num t0} t0 -> t0 -> t0) 
          (tmapVar1 : t0) 
          (tmapVar0 : t0) 
          : t0) 
        : t0 -> t0 -> t0) 
      (X : Tensor t0) 
      (Y : Tensor t0) 
      : Tensor t0) 
    : Tensor t0 -> Tensor t0 -> Tensor t0)
```

tnsorMap2Wedgeという新しい組み込み構文を追加する必要がある。
これはテンソルに添字を補完するときに違う添字を付加する。
Core.hsのWedgeApplyの実装を参考にこの違う添字の補完は実装できる。
この添字の補完はCore.hsでTensorMap2Wedgeを処理するときにおこなう。


```
def normalDot : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\X Y -> 
    ((. : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0) 
      (X : Tensor t0) 
      (Y : Tensor t0) 
      : Tensor t0) 
    : Tensor t0 -> Tensor t0 -> Tensor t0)
```

```
def wedgeDot : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0 := 
  (\X Y -> 
    (! (. : {Num t0} Tensor t0 -> Tensor t0 -> Tensor t0) 
      (X : Tensor t0) 
      (Y : Tensor t0) 
      : Tensor t0) 
    : Tensor t0 -> Tensor t0 -> Tensor t0)
```

こちらはWedgeApplyをそのまま保持する今の実装で良い。
Core.hsでWedgeApplyを処理するときに、XとYに違う添字を補完するようにする。
こちらはすでに実装できている。