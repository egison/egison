## 作業の目的

- docs以下のファイルで説明されている独自の言語機能を持つプログラミングEgisonについて静的型付けを実装すること。
- Egison上に実装されている数式処理システムの実用性を高めること

## 実装作業の手順の流れ

docs以下の論文や文章の内容を理解してください。
実装案について大事なことはdesign以下のファイルを参照してください。
また新しい案や案の更新があるときはdesign以下にまとめてください。

現在のテストコードを動かすのは、大規模な変更であるため無謀であるので、小さいテストを作成して動かす。
小さいテストはmini-testディレクトリを作り、そこの下に適宜生成。
mini-test以下のファイルには1-constant.egiのように数字を先頭に付加してください。
ライブラリをロードするときは、-lオプションを使う。
-tオプションを使うとmain関数を呼ばないテストができる。

% cabal run egison --  -t mini-test/34-tuple-matcher.egi

stackは使わずにcabalを使ってください。
コマンドは全て1分など時間制限をかけてタイムアウト(gtimeoutコマンドを使う)をするようにしてください。
コード内のコメントは英語で書いてください。
自動でgit commitするのはやめてください。

## 数式処理システムを更新した後の検査手順

数式処理システム（`lib/math/`、`hs-src/Language/Egison/Math/`、`hs-src/Language/Egison/Primitives/Arith.hs`、`declare rule auto`、`declare apply`、`declare mathfunc`、`declare derivative`、normalize/rewrite まわりなど）に手を入れたら、必ず以下を実行して **warning・error・slowdown** がないか確認すること。

### 1. 代表サンプルでの warning/error 検査

下記サンプルを `time` 付きで `-t` モードで実行し、

- `Type error:` / `Evaluation error:` 等のメッセージが出ていないか
- `Warning:` (Unbound variable、type assumption など) が出ていないか
- 実行時間が以前のベンチマークから極端に slow down していないか

を確認する：

```sh
time gtimeout 60 cabal run egison -- -t sample/math/geometry/riemann-curvature-tensor-of-S2.egi
time gtimeout 60 cabal run egison -- -t sample/math/algebra/cubic-equation.egi
time gtimeout 60 cabal run egison -- -t sample/math/algebra/quadratic-equation.egi
time gtimeout 60 cabal run egison -- -t sample/math/algebra/quartic-equation.egi
time gtimeout 60 cabal run egison -- -t sample/math/analysis/eulers-formula.egi
time gtimeout 60 cabal run egison -- -t sample/math/number/5th-root-of-unity.egi
time gtimeout 60 cabal run egison -- -t sample/math/number/7th-root-of-unity.egi
```

`cabal run egison -- -t <file>` は出力 1 行目だけでは error を見落とすので、
`2>&1 | head -20` などで先頭部分（型推論段階のエラーは最初に出る）を必ず確認する。

参考性能（`riemann-curvature-tensor-of-S2.egi` で `~5s` 程度、`cubic-equation` で `~4s`、`quartic-equation` で `~1.5s` 等）を上回る slow down があれば、何かが壊れているサインとみなす。

### 2. mini-test 一括検査

`mini-test/` 以下を全件回す。回帰がないか確認：

```sh
fail=0; pass=0; failed=""
for f in mini-test/*.egi; do
  gtimeout 30 cabal run egison -- -t "$f" >/dev/null 2>&1
  if [ $? -eq 0 ]; then pass=$((pass+1)); else fail=$((fail+1)); failed="$failed $f"; fi
done
echo "pass=$pass fail=$fail"
echo "$failed"
```

### 3. `cabal test` での warning/error 検査

`cabal test` でテストスイートを実行し、Egison プログラム実行時の warning・error が混入していないか確認：

```sh
gtimeout 600 cabal test 2>&1 | tee /tmp/cabal-test.log
grep -E "Warning:|Type error:|Evaluation error:|Unbound variable" /tmp/cabal-test.log | head -30
```

`grep` の結果が空になっていることが理想。何か出ていれば原因を特定して直す。

### 4. 設計ドキュメントの更新

数式処理システムの公開動作（`declare rule` の表現力、`declare apply` の意味論、`mathNormalize` の挙動、CAS タワーの正規化規則 等）に変更を加えた場合、`design/type-cas.md` の該当セクションも合わせて更新する。

### 検査をスキップしてよい変更

- コメント・リファクタリング (`hs-src/` 内のロジックに非可触)
- mini-test の追加 (新規テストの追加)
- `design/` 内のドキュメント変更
