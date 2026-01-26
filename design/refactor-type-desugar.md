型クラス展開とTensorMap挿入のコードを整理したい。

現在、型クラス展開の後にTensorMapを挿入しているが、逆にしたい。
TensorMapの挿入後に型クラス展開したい。
そのメリット:
型クラス展開時には、引数がスカラー型かテンソル型か確定しているため、型クラス展開時にunifyStrictを使えるようになる。

TensorMap挿入については、design/tensor-map-insertion-simple.mdに詳しく書いてある。

TensorMap挿入にはeta展開を事前にすることが必要か？
高階関数として、関数の引数に渡された関数についてはeta展開した方が良いかもしれない。

リファクタリング後も
cabal run egison -- -t sample/math/geometry/riemann-curvature-tensor-of-S2.egi
が問題なくTrueという文字列のみが出力されるようにしてほしい。

---

## 2026-01-26 調査結果

### 試した順序

1. **TensorMapInsertion → TypeClassExpand** (目標の順序)
   - 結果: リーマン曲率テンソルのテストで一部のassertionが失敗
   - 問題: 式が簡約されない (例: `1 - 2*(sin θ)^2 - (cos θ)^2` が `-(sin θ)^2` に簡約されない)
   - `scanAllTerms` デバッグ出力が大量に表示される

2. **EtaExpansion → TensorMapInsertion → TypeClassExpand**
   - 結果: 同様の問題が発生

3. **TypeClassExpand → TensorMapInsertion** (現在の動作順序)
   - 結果: すべてのテストがパス (11個のTrue)

### 現状の課題

順序を変更すると、テンソル計算における式の簡約が正しく行われなくなる。
原因の可能性:
- tensorMap2で包まれた式の中で型クラスメソッドが展開されると、
  簡約関数への参照が異なる形式になる可能性
- 式の構造が変わることで、既存の簡約ルールがマッチしなくなる可能性

### 今後の調査項目

1. 順序変更後の式の構造を詳しく調査
2. 簡約が失敗する具体的な箇所を特定
3. TypeClassExpandでunifyStrictを使用するための別のアプローチを検討