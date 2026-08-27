# Egison 処理系の実装構成

この文書は、ソースコードを読み込んでから評価するまでの現在の処理の流れを説明する。
各モジュールと主要関数の詳細な一覧は [FILE_MAPPING.md](./FILE_MAPPING.md) に集約する。

## 1. 全体の流れ

```text
ソースコード
  ↓ Phase 0: 構文解析
TopExpr
  ↓ Phase 1: load 展開
読み込み済み TopExpr 列
  ↓ Phase 2: 環境構築
型・クラス・インスタンス・パターン宣言の環境
  ↓ Phase 3–4: 構文糖衣の展開
ITopExpr / IExpr
  ↓ Phase 5–6: 型推論
TITopExpr / TIExpr
  ↓ Phase 7a: tensorMap 挿入
TIExpr
  ↓ Phase 7b: 型クラス辞書の展開
TIExpr
  ↓ Phase 8: 定義の再帰束縛と型情報の除去
IExpr の実行環境
  ↓ Phase 9: 評価
EgisonValue
```

`IExpr` は構文糖衣を除いた内部表現である。`TIExpr` は各節に `TypeScheme` を持つ型付き内部表現で、
型に基づく変換が終わるまで保持する。実行時に型が必要な専用節を内部表現へ明示的に残したうえで、
通常の型注釈は `stripType` で除去する。

## 2. Phase 0–2: 構文解析、読み込み、環境構築

`Parser/NonS.hs` がソースを `AST.hs` の `TopExpr` と `Expr` へ変換する。
`Eval.expandLoads` は `load` を展開し、同じファイルの重複読み込みを避ける。

`EnvBuilder.buildEnvironments` は、式本体の型推論より前に次の宣言を集める。

- 通常の型注釈とデータ構成子。
- 型クラス、インスタンス、メソッド型。
- パターンコンストラクタの型と matcher capability の情報。
- パターン関数の検査前 header。
- `declare symbol`、CAS 型エイリアス、CAS 部分型宣言。

パターン関数では、検査前の header と検査済みの `DualScheme` を別の環境に保存する。
これにより前方参照の名前解決と、検査済み定義の完全な具体化を混同しない。

## 3. Phase 3–4: 構文糖衣の展開

`Desugar.hs` は表層の `Expr` を `IExpr` へ変換する。中置演算子、短縮ラムダ、match-lambda、
クラス・インスタンス宣言などを内部表現へ展開する。

型クラス宣言とインスタンス宣言は、メソッド関数と辞書ハッシュへ変換する。辞書は
`__super_<Class>` の項目にスーパークラス辞書を保持する。

match 式と matcher literal のパターンマッチ処理そのものは、この段階で通常の条件分岐へ
展開しない。非自由データ型に対するバックトラックを含む照合は Phase 9 の評価器が行う。

## 4. Phase 5–6: 型推論

`Type/Infer.hs` は `IExpr` を推論しながら `TIExpr` を直接生成する。後から式と型の列を
位置で結び直す独立した「型付加」段階はない。

通常型のスキームは次の情報を分けて量化する。

```haskell
Forall [CapVar] [TyVar] [Constraint] Type
```

capability 変数と通常型変数は異なる sort、つまり別種の変数として代入する。通常型変数には
引数由来の A 変数と返り値由来の R 変数という用途の印があり、R 変数には
`MatcherSlot` を返り値位置へ含む型を代入できない。返された関数の仮引数位置にある
`MatcherSlot` は許される。

再帰定義では、自己参照へ fresh な R 変数を含む単相型を与え、本体の推論結果と単一化する。
循環する値定義の根は lambda または matcher literal に限る。

パターン型は capability と target の組 `Dual` として扱う。パターン関数では全引数と結果を
一つの `DualScheme` に一般化し、適用時も両 sort の量化変数を同時に具体化する。
Egison core と `type-pm-mech3` の境界は [type-pm-compatibility.md](./type-pm-compatibility.md)、
実装契約は [matcher-capability.md](./matcher-capability.md) を参照する。

## 5. Phase 7: 型に基づく変換

Phase 7 は型付き内部表現を保ったまま、次の順に二つの変換を行う。

1. `TensorMapInsertion.hs` がスカラー関数へのテンソル適用を `tensorMap`、`tensorMap2`、
   または Wedge 用の節へ変換する。
2. `TypeClassExpand.hs` が型クラスメソッドを具体的な辞書アクセスへ変換し、制約付き関数へ
   辞書引数を追加する。

tensor-lift を先に行うことで、型クラスのインスタンス選択は持ち上げ後の要素型を利用できる。
CAS の静的型だけでは辞書を選べない限定的な場合は、実行時用の `TIRuntimeDispatch` を残す。

型注釈が CAS 表現の選択を指示する場合は `IReshape` が残り、評価時に
`casReshapeAs` が値を指定された正規形へ組み替える。

## 6. Phase 8–9: 束縛と評価

`Eval.hs` は同じ読み込み単位の定義を再帰的に束縛し、実行用の環境を作る。
`Core.hs` は弱頭正規形を使う遅延評価、関数適用、IO、パターンマッチを実行する。

パターンマッチは matcher が返す照合状態を遅延列として探索する。型検査は matcher と slot の
静的な整合性を保証するが、候補の列挙順やバックトラックは評価器の責務である。
`match` と `matchDFS` の `else` はすべての通常節が空だった場合だけ元の環境で評価し、
通常節のパターン変数を引き継がない。型推論でも通常節の後に元の型環境で検査し、
通常節と同じ結果型を要求する。

`IRuntimeDispatch` は第1引数を一度だけ評価して CAS の浅い実行時型を調べ、選んだ型クラス辞書へ
適用する。詳細は [runtime-type-dispatch.md](./runtime-type-dispatch.md) を参照する。

## 7. 中間表現の役割

| 表現 | 主な段階 | 型情報 | 役割 |
|---|---|---|---|
| `TopExpr`, `Expr` | Phase 0–2 | 表層注釈のみ | ソース構文と宣言を保持する |
| `ITopExpr`, `IExpr` | Phase 3–4 | 節に必要な型だけ | 構文糖衣を除いた実行可能な形 |
| `TITopExpr`, `TIExpr` | Phase 5–7 | 各式に `TypeScheme` | 型推論結果と型に基づく変換を保持する |
| `EgisonValue`, `WHNFData` | Phase 9 | 原則として静的型なし | 実際の値と遅延評価状態 |

## 8. 調査用オプション

処理の途中を確認するときは次のオプションを使う。

```text
--dump-loads       load 展開後
--dump-env         環境構築後
--dump-desugared   IExpr への展開後
--dump-typed       型推論直後の TIExpr
--dump-ti          tensorMap 挿入後の TIExpr
--dump-tc          型クラス展開後の TIExpr
```

警告オプションと各段階の実装ファイルは [FILE_MAPPING.md](./FILE_MAPPING.md) にまとめる。
