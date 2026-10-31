# インタプリタの性能確認

## 計測方法

- 実行時統計は `+RTS -s` で，GC 時間，割り当て量，最大常駐量を確認する．
- ヒープ内訳は `+RTS -hT` でクロージャの種類ごとに確認する．
- 名前付き関数の適用回数は `--profile-calls` で集計する．終了時に上位60件を標準エラーへ出力する．
- コストセンターによる計測は，別のビルドディレクトリでプロファイル版を作成し，`+RTS -p` を使う．

```sh
gtimeout 300 cabal build --enable-profiling --profiling-detail=toplevel-functions --builddir=dist-prof exe:egison
```

同じ入力，コンパイラの版，最適化，実行時オプションで計測し，処理時間だけでなく出力の一致も確認する．Haskell のビルドや検証は同時に実行しない．

## 実装

`EvalState.callCounts` は関数の適用回数を保持する．集計を無効にした状態では回数表を作らない．関数名のスタックは `pushFuncName` と `popFuncName` が管理する．

文字列の連結は `concatString` と `intercalateString` を使う．`lib/core/string.egi` の `S.concat` と `S.intercalate` は，これらのプリミティブを参照する．

## 計測する処理

- `refVar` の環境探索と，深い束縛に対する参照の回数．
- `freeVarsInEnv` と型代入の合成に要する時間，割り当て量．
- パターン照合，関数適用，文字列出力に要する時間．
- 大きな数式の正規化と比較に要する時間．

性能結果は，入力と実行条件を添えて記録する．
