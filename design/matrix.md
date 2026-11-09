# 行列の逆行列と部分行列

実装は `lib/math/algebra/matrix.egi`，回帰テストは `test/lib/math/matrix.egi` と
`test/Test.hs` の `matrixBlockValidationTests` にある．

## 逆行列

`M.inverse : Matrix MathValue -> Matrix MathValue` は，入力が二つの軸を持つテンソル
（階数2）で，行数と列数が等しいことを検査する．1×1行列では唯一の成分の逆数を返す．
2×2以上では，結果の `(i,j)` 成分を求める
とき，入力の第 `j` 行と第 `i` 列を除いた `(n-1)×(n-1)` 行列を `generateTensor` で直接
生成する．その行列式に符号 `(-1)^(i+j)` を付け，入力の行列式で割る．除く行と列の順序は，
余因子行列の転置に対応する．

この計算は空の範囲参照を使わない．`M.determinant` とその別名 `M.det` も，入力が階数2の
正方行列であることを検査する．テンソルの軸長を正整数とする規則はそのまま適用する．

## 空の部分行列を表すデータ

`MatrixBlock a` は，行数・列数と，行の順に並べた成分列を保持する通常のデータ型である．
テンソルとは異なり，0行または0列を許す．例えば，`M.block 0 3 []` と `M.block 3 0 []` は，
どちらも成分を持たないが，異なる寸法を保持する．

```egison
inductive MatrixBlock a := MatrixBlock Integer Integer [a]
```

構築には `M.block rows cols cells` を使う．行数と列数は非負整数であり，成分数はその積に
一致しなければならない．直接データ構築子を使った値も，公開操作で読み出す際に検査する．

| 操作 | 結果・条件 |
| --- | --- |
| `M.blockShape block` | `[行数, 列数]`．空の場合も両方を保持する． |
| `M.blockElements block` | 行の順に並べた成分列． |
| `M.blockParts block` | `(行数, 列数, 成分列)`． |
| `M.blockRows block` | 行ごとの成分列．3×0なら `[[],[],[]]`． |
| `M.blockAt block row col` | 1から数えた位置の成分．範囲外はエラー． |
| `M.blockSlice block row col height width` | 開始位置と行数・列数で指定した部分行列．0行・0列を許し，開始位置と終端が元の範囲を超える場合はエラー． |
| `M.toBlock tensor` | 階数2のテンソルの寸法と成分を取り出す．添字名や共変・反変の情報は保持しない． |
| `M.toTensor block` | 両方の寸法が正の場合に，添字名のない行列テンソルを生成する．0行・0列はエラー． |
| `M.blockDet block` | 正方形の部分行列の行列式．0×0の場合は1．正方形以外はエラー． |

## 分解と結合

`matrix` は `MatrixBlock MathValue` のパターンマッチを定めるマッチャーである．
テンソルを分解するときは，まず `M.toBlock` を適用する．

- `emptyBlock rows cols` は，行数または列数が0の部分行列に一致する．
- `quadCons a B C D` は，左上の成分 `a`，同じ行の残り `B`，同じ列の残り `C`，
  残りの行と列からなる部分行列 `D` に分解する．空の部分行列には一致しない．
- `matCons #i #j a A B C D` は，第 `i` 行・第 `j` 列の成分 `a` と，その行と列を除いた
  左上 `A`，右上 `B`，左下 `C`，右下 `D` に分解する．範囲外の位置には一致しない．
- 値パターンは行数・列数・成分列を比較する．

部分行列にも同じ `matrix` を使うため，`quadCons` や `matCons` は入れ子にできる．
境界の部分行列には0行や0列が現れるが，テンソルの範囲参照は必要ない．

`M.join A B C D` は，`A B` を上段，`C D` を下段として並べた `MatrixBlock a` を返す．
上段同士・下段同士の行数，左側同士・右側同士の列数が一致することを検査する．
結果が空でも行数・列数を保持する．テンソルが必要なら，結果に `M.toTensor` を適用する．

```egison
-- Remove the first row and second column, then construct a tensor.
match M.toBlock [|[|1,2,3|],[|4,5,6|],[|7,8,9|]|] as matrix with
  | matCons #1 #2 _ $A $B $C $D -> M.toTensor (M.join A B C D)
-- [|[|4,6|],[|7,9|]|]
```

## 検証

回帰テストは，数値・記号を成分とする1×1，2×2，3×3の逆行列，非対称行列での転置，
左右の積が単位行列になること，分解の全ての端・隅・中央，入れ子の分解，0行・0列の
寸法保存，分解後の結合を検査する．不正な寸法，成分数，変換，添字，結合は実行時エラーの
診断を検査する．S2・T2の曲率テンソルを計算する公開例も全体テストに含む．
