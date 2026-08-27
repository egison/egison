# 拡張可能 CAS タワーの実装マッピング

この文書は、拡張可能な CAS 型タワーの現行仕様と実装箇所の対応を示す。
商型の意味論は
[type-cas-quotient.md](./type-cas-quotient.md)、利用者向けの総合仕様は
[type-cas.md](./type-cas.md) を参照する。

## 1. 基本原則と実装上の帰結

| 決定 | 現行実装 |
|---|---|
| 順序は一意な最小上界を持つ | 宣言時に有限な節点集合上で半束性を検査し、推論時の join に同じ順序を使う |
| 型ごとの書き換え規則を導入しない | `declare rule` は `MathValue` の大域規則として扱う |
| タワーの型名は透明エイリアス | `declare cas-type` を登録時に展開し、単一化へ新しい不透明型の規則を加えない |
| 商はタワー外 | `declare cas-quotient` は不透明な型を導入し、CAS 部分型順序へ参加させない |
| 型ごとの埋め込み関数を持たない | 注釈から挿入した `IReshape` と `casReshapeAs` が表現の組み替えを一元的に行う |

## 2. `declare cas-type` の登録

表層構文は次の形である。

```egison
declare cas-type GaussianInt := Poly Integer [i]
declare cas-type GaussianPoly := Poly GaussianInt [x]
```

`Parser/NonS.hs` が `DeclareCasType` を作り、`EnvBuilder.collectCasTypeAlias` が読み込み単位の
全宣言を検査して登録する。

- 名前は大文字で始める。
- 組み込み CAS 型、inductive 型、同じ読み込み単位の別名と衝突できない。
- 宣言順に依存しないよう固定点で解決するため、前方参照を許す。
- 自己参照と循環参照は、未解決の別名を示す型エラーにする。
- 多段の別名は登録時に完全展開し、型推論と単一化には構造型を渡す。

別名環境は `EvalState.casTypeAliasEnv` に保存し、後から読み込んだファイルでも利用できる。
型注釈、クラス・インスタンス、パターン関数、`declare symbol` など、型式を通常型へ変換する
全ての入口で同じ別名展開を使う。

## 3. `declare cas-subtype` と join

```egison
declare cas-subtype A <: B
declare cas-subtype A ⊂ B
```

二つの表記は同じ宣言である。`Type/Subtype.hs` は次を一つの順序として扱う。

- 組み込み CAS タワーの構造的な部分型規則。
- 係数型と原子集合を含む構造的な伝播規則。
- ユーザが宣言した部分型辺。

`isSubtypeWith` と `joinTypesWith` は同じ順序を参照する。宣言時の `checkEdgeAddition` は、
冗長辺、循環、複数の最小上界による曖昧性、既存 join の細分化を区別する。曖昧な辺は拒否し、
一意な最小上界にするための候補辺を診断へ含める。

半束性の検査は、組み込みの代表型と宣言に現れた有限個の節点上で行う。任意の型式全体に対する
完全な判定ではない。この有限近似は現在の仕様上の制限である。

宣言辺は `EvalState.casSubtypeEdges` に保存し、各トップレベル式の推論状態へ渡す。
関数適用で複数の CAS 型を合わせる場合、推論器はこの join を使って結果型を決める。
型クラスの最具体インスタンス選択と実行時型ディスパッチも同じ順序を使う。

## 4. `reshape` と正規形

型注釈が CAS 型間の表現選択を指示すると、型に基づく変換が `IReshape target expression` を残す。
評価器は CAS 値に対して `Math/CAS.hs` の `casReshapeAs` を呼ぶ。

```egison
def p : Poly (Poly Integer [i]) [x] := expression
```

通常の算術結果は、入れ子の多項式係数を外側へ分配した平らな正規形に戻す。
`casNormalizePolyWith FlattenNested` がこの規則を実装する。`reshape` の最終段階だけは
`KeepNested` を使い、注釈が要求した係数の入れ子を保持する。

したがって、入れ子表現は `reshape` 直後にだけ存在し、算術演算を通ると既定の平らな表現へ戻る。
異なる表現から来た同類項も同じ正規形で併合される。`reshape` は値を変えず表現だけを選ぶため、
途中で別の表現を経由しても最終対象が同じなら同じ結果になる。

## 5. `declare cas-quotient`

商型はタワーの透明エイリアスではなく、不透明な型として導入する。

```egison
declare cas-quotient Mod7 := Integer by (\x -> i.modulo x 7)
```

`Eval.expandCasQuotientDecls` は宣言を通常のトップレベル定義へ展開する。

- `reduce`, `proj`, `repr` の関数。
- 加法・乗法・等値性の型クラスインスタンス。
- `reduce` の冪等性と加法・乗法への合同性を確認する assertion。

商型名は不透明な `TInductive` として登録されるため、同名の商型とだけ単一化し、
`declare cas-subtype` の対象にはならない。タワーとの横断は生成された `proj` と `repr` を明示的に使う。

## 6. 推論とインスタンス選択への接続

CAS の join は一般の HM 単一化を置き換えない。関数適用など、CAS の複数表現を一つの結果型へ
そろえることが意味的に許される位置だけで使う。非 CAS 型の不一致や、商型と基底型の混在を
join で救済しない。

インスタンス選択は `Type/Instance.hs` が候補を照合し、`Type/Subtype.hs` の順序で最も具体的な
候補を選ぶ。順序上は同値でも正規形が異なる候補が残る場合、対象型と外側の型構成子が一致する
候補を優先する。

## 7. 多項式 GCD 簡約の現在の範囲

`Math/CAS.hs` の `univariateGcdReduce` は、同じ一つのシンボルを持つ分子・分母の多項式を、
有理数係数上の最大公約多項式で約分する。正の指数と、整数または整数分数の係数だけを対象とし、
条件を満たさない入力は変更しない。

多変数や代数関係を含む簡約は、この関数へ無理に広げず、
[cas-simplification.md](./cas-simplification.md) のグレブナー基底とイデアル正規形を使う。

## 8. 検収

[cas-tower-usecases/](./cas-tower-usecases/) の 8 本を実行可能な検収基準とする。

1. 型エイリアス。
2. Gaussian 整数。
3. 部分型昇格。
4. 入れ子の Gaussian 多項式。
5. 二次拡大。
6. 複数拡大の join。
7. 商型による剰余算術。
8. join の完備化。

商の合成による有限体は `test/lib/math/quotient-field.egi`、CAS の一般回帰は
`test/lib/math/` で検証する。
