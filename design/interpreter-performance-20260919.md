# インタプリタ性能の調査と改善(2026-09-19)

Formurae の正規化(FME → Egison → FEIR)が 12〜30 分かかる問題を調べた記録。
対象は `formurae/examples/kinetic_transport`(生成された Egison 単位 1 MB、FEIR 1.8 MB)と
`kinetic_free_surface`(2.3 MB、FEIR 6.5 MB)。計測はすべて Apple M5、GHC 9.10.1、`-O1`。

## 計測方法

- 実行時統計: `+RTS -s`(GC 時間、割り当て量、最大常駐)。
- ヒープ内訳: `+RTS -hT`(クロージャ種別、プロファイル版ビルド不要)。
- コストセンター: `cabal build --enable-profiling --profiling-detail=toplevel-functions --builddir=dist-prof exe:egison`
  で別ディレクトリにプロファイル版を作り `+RTS -p` で実行。
- Egison レベルの関数呼び出し回数: 新設した `--profile-calls`(下記)。

## 判明したこと

1. **時間の 97% は評価段階**(`main []` の実行)で、構文解析・型検査は 2% 未満(kinetic_transport)。
   2.3 MB の単位でも読み込みは 35 秒(うち GC 21 秒、常駐 6.7 GB、大半がサンク)で、
   評価段階の数分〜数十分に比べれば小さい。
2. 評価段階では CAS 本体(`casNormalize` 540 万回、`casMult` 9 万回)は 0.1% 未満。
   時間はインタプリタの一般的な経路に分散していた。
   - 関数名スタック(`mLabelFuncName` / `pushFuncName` / `popFuncName`): 23.5%。
     エラーメッセージ用に関数適用ごとに `get`/`put` でレコード全体を複製し、
     多相な `MonadEval m` 経由の辞書渡しで結合していた。
   - 変数参照 `refVar`: 8.5%(2.8 億回)。環境の層を順に `Map String` で引く。
   - パターンマッチ機構(`inductiveMatch`、`primitivePatPatternMatch`、`makeBindings` など): 約 30%。
3. 1 MB の単位で 1.07 TB を割り当てていた(GC は 12%)。割り当ての多くは
   Formurae ライブラリ側のアルゴリズムに起因する。`--profile-calls` で
   `list` マッチャの適用 3,000 万回、`foldr` 960 万回、`FEIR.render` 124 万回、
   `FEIR.escapeChar` 140 万回などが見えた。s 式の並べ替え・述語の定数判定・
   文字列の引用が s 式を繰り返し再描画していた。
4. `Eq EgisonValue` の `CASData` 比較は両辺を `casNormalize` してから比べる。
   小さい値では問題にならないが、大きい多項式を記号と比較する用途では無駄になる
   (Formurae 側で記号のときだけ比較するよう変更した)。

## 変更(Egison)

- `EvalState.pushFuncName`/`popFuncName`: `modify'` による厳格な更新にし、
  `mLabelFuncName` と両インスタンスのメソッドに `INLINE` を付けた。
  kinetic_transport の正規化は 218 秒 → 133 秒(出力は同一)。
- `--profile-calls`: 名前付き関数の適用回数を集計し、終了時に上位 60 件を stderr へ出す
  開発用オプション。`EvalState.callCounts`(`Maybe (HashMap String Int)`)で、
  無効時の追加コストは分岐 1 回。
- 文字列プリミティブ `concatString`(`Text.concat`)と `intercalateString`
  (`Text.intercalate`)を追加し、`lib/core/string.egi` の `S.concat`、`S.intercalate` を
  これらの別名にした。従来は `foldr` と `appendString` の組み合わせで、
  リストの要素数と文字列長の両方に比例する Egison レベルの処理だった。

## 変更(Formurae ライブラリ `lib/formurae-feir.egi`)

- 場の登録簿(1,220 件)を関数出現ごとに線形走査していた `FEIR.fieldHeadMatches` を、
  登録時に前計算した名前と添字による文字列比較に置き換えた(登録簿の組を 4 要素から 7 要素へ)。
- `FEIR.encodeScalar` / `encodeFactor` で、記号でない値に対する `= π` と
  パラメータ・座標の照合を省略した(いずれも両辺を正規化する比較)。
- s 式の並べ替え(`insertSExpr` / `sortSExprs` / `uniqueSortedSExprs`)は
  各要素を一度だけ描画した鍵で比較する。述語の定数判定は構造比較にした。
- `FEIR.quoteString` は制御文字を含まない文字列(`regex "[[:cntrl:]]"` で判定)に
  `show` を使う。`show` は引用符・バックスラッシュ・改行・復帰・タブを FEIR と
  同じ形で逃がし、制御文字だけ表記が異なる(FEIR は `\u{..}`)ため、その場合のみ
  従来の文字単位の経路を使う。

## 結果(出力の FEIR はすべてバイト単位で同一)

| モデル | 変更前 | 変更後 |
|---|---|---|
| kinetic_transport(1 MB 単位) | 270 秒 | 68 秒 |
| kinetic_surface | (未計測) | 70 秒 |
| elastic_shell | 48 秒(2026-09-12 の記録) | 9 秒 |

`kinetic_free_surface` の本番正規化の値は追記する。

## 残る課題(未実施)

- `refVar` の層走査: 変数参照を脱糖時に(深さ, 位置)へ解決すれば大幅に減るが、
  環境表現の変更を伴う大きな改修。
- パターンマッチ機構の一般的なコスト: ライブラリ側で `match` の回数を減らすのが現実的。
- 型検査: 定義ごとに環境全体の自由型変数を走査する(`freeVarsInEnv`、定義数の二乗)。
  `composeSubst` が `Map.map` を遅延させ、置換の合成がサンクの連鎖になる。
  読み込みが全体の 2% 未満のため今回は手を付けていない。
- 実行時オプション: 読み込み段階では `-A1G` で 35 秒 → 23 秒、`-N4 -A256M -qn4` で 21 秒。
  Formurae の `tools/run_egison_machine.sh` は `EGISON_RTS_OPTS` で追加オプションを渡せる。
