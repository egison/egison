## Pretty print関連

dump-tiのpretty printについて
Hash String [a] -> [a] -> Bool
のような出力を
Hash String ([a] -> [a] -> Bool)
に直したい。

dump-envのpretty printについて、
delete : ∀a. Eq a => a -> [a] -> [a]
のような出力を
delete : {Eq a} a -> [a] -> [a]
に直したい。

## Type class関連

```
instance {Eq a} Eq [a] where
  (==) xs ys :=
    match (xs, ys) as (list something, list something) with
      | ([], []) -> True
      | ($x :: $xs', $y :: $ys') -> x == y && xs' == ys'
      | _ -> False
  (/=) xs ys := not (xs == ys)
```
上記は現在下記のように展開される。
```
def eqCollectiona := {| ("eq", eqCollectionaEq), ("neq", eqCollectionaNeq) |}
def eqCollectionaEq := \xs ys ->
  match (xs, ys) as (list something, list something) with
      | ([], []) -> True
      | (:: x xs', :: y ys') -> && (== x y) (== xs' ys')
      | _ -> False
def eqCollectionaNeq := \xs ys -> not (== xs ys)
```
そして、以下のような型がつく。
```
eqCollectionaEq : [a] -> [a] -> Bool
eqCollectionaNeq : [a] -> [a] -> Bool
```

上記の問題は{Eq a}という制約が失われているところにある。
また、型パラメータであるaが辞書や関数の名前に入るのも良くない。
実際は以下のように展開されるべきである。
```
def eqCollection := {| ("eq", eqCollectionaEq), ("neq", eqCollectionaNeq) |}
def eqCollectionEq := \xs ys ->
  match (xs, ys) as (list something, list something) with
      | ([], []) -> True
      | (:: x xs', :: y ys') -> && (== x y) (== xs' ys')
      | _ -> False
def eqCollectionNeq := \xs ys -> not (== xs ys)
```
そして以下のような型がつくべきである。
```
eqCollectionEq : {Eq a} [a] -> [a] -> Bool
eqCollectionNeq : {Eq a} [a] -> [a] -> Bool
```