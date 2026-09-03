# test/type-error — 型検査の拒否(reject)適合テスト

このディレクトリの各 `.egi` ファイルは、**型検査がエラーを検知すべき**プログラムを
1ファイル1ケースで収めたものです。`mini-test/` が受理側(型 clean + 実行結果)の
回帰テストであるのに対し、こちらは拒否側の適合テストです
（現行仕様は `design/matcher-capability.md` の reject 側検収を参照）。
各ファイルの先頭コメントに、対応する論文の規則と期待されるエラーを記しています
(規則名は type-pm-paper 論文1の生成規則 G-Match・G-Literal・Q-*・P-* と、
pattern function の規則 PATFUN-DEF・PAT-APP)。

## 検証方法

`-t` は permissive モード(型エラーを出しても untyped 評価にフォールバックして exit 0)
なので、**exit code ではなく出力の `Type error:` を grep** して判定します。
全ファイルが「`Type error:` を含み、`Parse error` を含まない」ことが合格条件です:

```sh
fail=0
for f in test/type-error/*.egi; do
  o=$(gtimeout -k 10 60 cabal run -v0 egison -- --type-check-strict -t "$f" 2>&1)
  printf '%s\n' "$o" | grep -q -E "Type error:|^Error:" || { echo "MISSING ERROR: $f"; fail=1; }
  printf '%s\n' "$o" | grep -q "Parse error" && { echo "PARSE ERROR: $f"; fail=1; }
done
[ $fail -eq 0 ] && echo "all rejected as expected"
```

(`printf` を使うのは、zsh の `echo` がメッセージ中の `\x` を解釈してしまうため。
`93`–`96` のシグネチャ境界エラーは環境構築時に `Error:` として報告される。)

かつての 57・64(積 matcher 型の application／変数を 2 hole に渡す)は、
matcher 型と matcher のタプル型を同一視する正準化のもとで受理されるのが正しく、
`test/lib/core/next-matcher-components.egi` の受理側回帰に移した。68(節の欠けた
構築子)は根の網羅性が `--matcher-consistency-warnings` の警告で報告される事項に
なったため削除した。

## ケース一覧

| ファイル | 規則 / 出典 | 内容 |
|---|---|---|
| 01-something-cons | pattern と matcher の capability 等式(G-Match / P-Cons) | `something` × cons パターン |
| 02-something-cons-param | 同 | 関数パラメータ経由、**適用点**で拒否 |
| 03-nested-ctor-element | ネスト構築子の capability 等式(P-Cons) | `multiset something` × ネスト構築子 `num` |
| 04-something-tuple-pattern | タプル pattern の capability 等式(P-Pair) | `something` × タプルパターン(積 capability の要求) |
| 05-matcher-target-mismatch | target 等式(G-Match) | `num` パターン × `[Integer]` |
| 06-target-type-mismatch | G-Match の target 側 | `matchAll 5 as multiset integer` |
| 10-patfun-body-structural | PAT-APP capability 側・本体 | `pair $x []` × `something` |
| 11-patfun-arg-structural | PAT-APP capability 側・引数 | `idp ($x :: $xs)` × `something` |
| 12-patfun-nested-arg-structural | PAT-APP capability 側・ネスト引数 | `pair (num $n) []` × `multiset something` |
| 13-patfun-target | PAT-APP ターゲット側 | `seqp`(`[Tile]`)× `[Integer]` ターゲット |
| 14-patfun-arg-target | PAT-APP 引数ターゲット | `seqp #1 _`(`Integer` vs `Tile`) |
| 20-patfun-linearity-unused | PATFUN-DEF 線形性 | 未使用パラメータ |
| 21-patfun-linearity-order | 同 | 宣言順違反 |
| 22-patfun-linearity-dup | 同 | 重複使用 |
| 23-patfun-linearity-under-or | 同 | or 分岐配下での使用 |
| 24-patfun-nested-matcher-slot | PATFUN-DEF / G-Literal | value pattern 式内の matcher literal でも hole の要求と next matcher の等式を検査 |
| 30-value-pattern-expr-type | P-Value | 値パターン内式の型エラー(`x ++ [1]`) |
| 31-nonlinear-target-type | 非線形パターン(P-Value の束縛参照) | `$x :: #x`(要素 vs リスト) |
| 32-or-pattern-bindings | PAT-OR | 分岐間の束縛変数不一致 |
| 40-matcher-next-structural | G-Literal の hole 要求(Q-Cons) | 構築子頭 hole への `something` |
| 41-matcher-body-matchsite | 本体内 match-site 検査 | matcher 本体内の `integer` × cons |
| 42-tuple-pattern-arity | PAT-TUPLE | タプルパターンの arity 不一致 |
| 50-matcher-collection-hetero | capability equality | `[something, list integer]`(`Any` と `[Any]` の異種 capability) |
| 51-matcher-cast-structured | capability equality | `Any` と `[Any]` という ground capability の不一致 |
| 52-missing-signature-constraint | シグネチャ完全性(残存制約検査) | 本体が `<=`({Ord a})を要求するのにシグネチャに無い |
| 53-matcher-alias-specialize | capability equality | `eq` の ground `Any` と `Integer` capability の不一致 |
| 54-something-structured-hole | G-Literal の hole 要求 | 結果注釈に関わらず，list tail hole への `something` を拒否 |
| 55-multisite-target-conflict | λ束縛の単相性(複数 match site) | λ束縛 matcher を `[Integer]` と `[String]` の2 site で使用(単相なので拒否) |
| 56-multisite-structural-join | 同(capability 等式) | site 1 の cons 要求で matcher の capability が `[χ]` に決まり、`g something` が適用点で拒否 |
| 58-next-matcher-slot-structural | G-Literal の hole 要求 | target は一致するが capability が不一致の next matcher |
| 59-next-matcher-bare-variable | G-Literal の hole 要求 | 変数形の `Any` matcher を構造 hole へ渡す |
| 60-next-matcher-bare-application | 同 / 構文非依存 | application 形の同じ `Any` capability を構造 hole へ渡す |
| 61-next-matcher-bare-lambda | 同 / 構文非依存 | lambda application 形の同じ `Any` capability を構造 hole へ渡す |
| 62-next-matcher-nested-rename | nested capability equality | `Maybe [p]` の入れ子骨格を欠く matcher |
| 63-next-matcher-zero-hole | G-Literal(0 hole の next matcher は `()`) | 0 hole に `()` 以外の next matcher |
| 65-next-matcher-slot-target | G-Literal の hole 要求 | capability は一致するが target が不一致の next matcher |
| 66-next-matcher-repeated-slot | capability 等式 | 同じ型引数から射影された二つの hole に `Choice` と `Any` の capability が届く不一致 |
| 67-target-specialization-cons | capability/target separation | target を list へ特殊化した `Any` matcher で cons を拒否 |
| 69-exact-clause-mismatch | 共有 capability の等式(G-Literal) | 同じ literal の clause 間で header capability が不一致 |
| 72-capability-unknown-former | capability name elaboration | 未宣言 head を `Capability kind error` で拒否 |
| 73-capability-arity-mismatch | capability kind elaboration | user inductive head の arity 不一致を `Capability kind error` で拒否 |
| 74-capability-alias-head | canonical former discipline | transparent surface type alias の capability head 使用を `Capability kind error` で拒否 |
| 75-local-capability-unknown-former | local annotation elaboration | 式内注釈からの未宣言 capability head bypass を `Capability kind error` で拒否 |
| 80-capability-builtin-collision | frozen former signature | builtin と同じ canonical ID を持つ user inductive を `Capability kind error` で拒否 |
| 82-any-cannot-witness-capability | G-Match(gradual `Any` の境界) | 型 `Any` の matcher 式を構造 pattern の capability の証拠にせず拒否 |
| 83-ordinary-annotation-rigidity | scheme annotation checking | `forall a. a -> a` の通常型 binder を rigid にし，定数を返す過剰一般注釈を拒否 |
| 84-nested-annotation-rigidity | nested annotation checking | top-level の rigid 通常型 binder を nested annotation でも共有し，特殊化を拒否 |
| 85-pattern-function-annotation-rigidity | pattern-function checking | pattern function の明示通常型 binder を rigid に検査する |
| 86-pattern-function-nested-annotation-rigidity | nested pattern annotation | pattern-function body の nested annotation でも同じ rigid binder を共有する |
| 87-capability-annotation-rigidity | capability annotation checking | `Matcher p Integer` の capability binder を rigid にし，`something` による `Any` への特殊化を拒否 |
| 88-patfun-param-target | PATFUN-DEF / PAT-EMBED target | `~parameter` が宣言された `Integer` target を保持し，結果の `Bool` へ再型付けされることを拒否 |
| 89-patfun-exact-arity | PAT-APP exact arity | target-only の関数型統一では受理できる pattern function の過少適用を拒否 |
| 90-closed-field-slot-application | G-Literal の hole 要求 | list field の hole が要求する `Matcher [Any] [Integer]` へ `something` を渡す適用を拒否 |
| 91-match-else-result | match result checking | 通常節と `else` の結果型不一致を拒否 |
| 92-match-else-scope | match fallback scope | 通常節の束縛を `else` から参照することを拒否 |
| 93-data-constructor-open-scheme | signature closedness | data constructor field の未宣言型変数を拒否 |
| 94-data-constructor-undetermined-capability | parameters determined | data constructor field にだけ現れる capability 変数を拒否 |
| 95-pattern-constructor-undetermined-capability | parameters determined | pattern constructor field にだけ現れる capability 変数を拒否 |
| 96-pattern-function-open-scheme | signature closedness | pattern function の未宣言型変数を拒否 |

## ケース追加時の注意

- 1ファイル1ケース。先頭コメントに対応規則と期待エラーを書く。
- 追加時は必ず実行して、**意図したエラーで**拒否されることを確認する
  (無関係なエラーや parse error で偶然 reject されると回帰検出にならない)。
- 72--75 と 80 は共通の `Type error:` に加えて `Capability kind error` を含むことを
  確認する。これにより，後段の型推論エラーによる偶然の reject と区別する。
- 82 は `Any cannot witness a structured matcher capability` を含むことを確認する。
- 83--86 は `TSkolem`，87 は capability skolem の surface 表示 `Matcher $skc` を
  含むことを確認し，別の型エラーで偶然 reject されていないことを確認する。
- 88 は `Integer` と `Bool`，89 は `expects 2 arguments, but got 1` を含むことを
  確認し，pattern function の別の検査で偶然 reject されていないことを確認する。
- 90 は `matcher capabilities do not unify` を含むことを確認し，list field の定義自体
  ではなく，hole が要求する matcher 型への不適合な適用で reject されたことを確認する。
- 受理側の対になるケースがあれば `mini-test/` に置く
  (例: `mini-test/120-patfun-struct-index.egi`)。

再帰 matcher の受理側は `test/lib/core/ar-recursive-matcher.egi` と、strict mode で
読み込む `test/lib/core/ar-recursive-matcher-strict.egi` に置く。直接参照、局所 alias、
通常の関数適用を同じ等式規則で検査し、式の名前や値の依存経路を理由とする
reject ケースは設けない。
