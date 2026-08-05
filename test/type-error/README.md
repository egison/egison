# test/type-error — 型検査の拒否(reject)適合テスト

このディレクトリの各 `.egi` ファイルは、**型検査がエラーを検知すべき**プログラムを
1ファイル1ケースで収めたものです。`mini-test/` が受理側(型 clean + 実行結果)の
回帰テストであるのに対し、こちらは拒否側の適合テストです
(`design/paper-compliance-roadmap.md` 課題 H の reject 側)。
各ファイルの先頭コメントに、対応する論文の規則・出典と期待されるエラーを記しています。

## 検証方法

`-t` は permissive モード(型エラーを出しても untyped 評価にフォールバックして exit 0)
なので、**exit code ではなく出力の `Type error:` を grep** して判定します。
全ファイルが「`Type error:` を含み、`Parse error` を含まない」ことが合格条件です:

```sh
fail=0
for f in test/type-error/*.egi; do
  o=$(gtimeout -k 10 60 cabal run -v0 egison -- -t "$f" 2>&1)
  echo "$o" | grep -q "Type error:" || { echo "MISSING ERROR: $f"; fail=1; }
  echo "$o" | grep -q "Parse error" && { echo "PARSE ERROR: $f"; fail=1; }
done
[ $fail -eq 0 ] && echo "all rejected as expected"
```

## ケース一覧

| ファイル | 規則 / 出典 | 内容 |
|---|---|---|
| 01-something-cons | COERCE-MATCHER-TO-SLOT(論文 B Case 2) | `something` × cons パターン |
| 02-something-cons-param | 同(B Case 3) | 関数パラメータ経由、**適用点**で拒否 |
| 03-nested-ctor-element | PAT-CON の capability 伝播(B.2.3) | `multiset something` × ネスト構築子 `num` |
| 04-something-tuple-pattern | MS Progress のタプルケース | `something` × タプルパターン(積型頭の slot) |
| 05-matcher-target-mismatch | ターゲット不一致(B.2.1) | `num` パターン × `[Integer]` |
| 06-target-type-mismatch | T-MATCHALL のターゲット側 | `matchAll 5 as multiset integer` |
| 10-patfun-body-structural | PAT-APP capability 側・本体(レビュー反例 M1) | `pair $x []` × `something` |
| 11-patfun-arg-structural | PAT-APP capability 側・引数(M1) | `idp ($x :: $xs)` × `something` |
| 12-patfun-nested-arg-structural | PAT-APP capability 側・ネスト引数(B.2.3) | `pair (num $n) []` × `multiset something` |
| 13-patfun-target | PAT-APP ターゲット側 | `seqp`(`[Tile]`)× `[Integer]` ターゲット |
| 14-patfun-arg-target | PAT-APP 引数ターゲット(B.1.2) | `seqp #1 _`(`Integer` vs `Tile`) |
| 20-patfun-linearity-unused | PATFUN-DEF 線形性(M2) | 未使用パラメータ |
| 21-patfun-linearity-order | 同 | 宣言順違反 |
| 22-patfun-linearity-dup | 同 | 重複使用 |
| 23-patfun-linearity-under-or | 同 | or 分岐配下での使用 |
| 24-patfun-nested-matcher-slot | PATFUN-DEF / R12 | value pattern 式内の matcher literal でも deferred slot 検査を完遂 |
| 30-value-pattern-expr-type | PAT-VALUE(B.1.1) | 値パターン内式の型エラー(`x ++ [1]`) |
| 31-nonlinear-target-type | 非線形パターン(B.1.1) | `$x :: #x`(要素 vs リスト) |
| 32-or-pattern-bindings | PAT-OR | 分岐間の束縛変数不一致 |
| 40-matcher-next-structural | Def 4.2(1a) / PP-Con(B の `weird`) | 構築子頭 hole への `something` |
| 41-matcher-body-matchsite | 本体内 match-site 検査 | matcher 本体内の `integer` × cons |
| 42-tuple-pattern-arity | PAT-TUPLE | タプルパターンの arity 不一致 |
| 50-matcher-collection-hetero | capability equality | `[something, list integer]`(`none` と `[none]` の異種 capability) |
| 51-matcher-cast-structured | capability preservation | `def m2 : Matcher [none] [Integer] := something`(capability 強化の拒否) |
| 52-missing-signature-constraint | シグネチャ完全性(残存制約検査) | 本体が `<=`({Ord a})を要求するのにシグネチャに無い |
| 53-matcher-alias-specialize | capability preservation | `def myint : Matcher Integer Integer := eq`(`none` からの capability 強化を拒否) |
| 54-something-structured-hole | PP-Con 遅延判定 | 結果注釈を capability evidence に使わず，list tail hole への `something` を拒否 |
| 55-multisite-target-conflict | Algorithm W Step 3a(複数 match site) | λ束縛 matcher を `[Integer]` と `[String]` の2 site で使用(単相なので拒否) |
| 56-multisite-structural-join | 同(capability 要求の join) | site 1 の cons 要求が commit 済み slot に残り、`g something` が適用点で拒否 |
| 57-next-matcher-nontuple | T-MATCHER / R12 成分境界 | 2 hole に積を返す非 tuple application |
| 58-next-matcher-slot-structural | Algorithm W Step 3a / R12 | target は一致するが capability が不一致の既存 slot |
| 59-next-matcher-bare-variable | COERCE-MATCHER-TO-SLOT / R12 | 変数形の `none` matcher を構造 hole へ渡す |
| 60-next-matcher-bare-application | 同 / 構文非依存 | application 形の同じ `none` capability を構造 hole へ渡す |
| 61-next-matcher-bare-lambda | 同 / 構文非依存 | lambda application 形の同じ `none` capability を構造 hole へ渡す |
| 62-next-matcher-nested-rename | capability rename / R12 | `Maybe [p]` の入れ子骨格を欠く matcher |
| 63-next-matcher-zero-hole | T-MATCHER / R12 成分境界 | 0 hole に `()` 以外の next matcher |
| 64-next-matcher-product-variable | T-MATCHER / R12 成分境界 | 積 matcher 型の変数を2 holeへ暗黙分解 |
| 65-next-matcher-slot-target | Algorithm W Step 3a / R12 | capability は一致するが target 添字が不一致の既存 slot |
| 66-next-matcher-repeated-slot | exact merge / R12 | 同一 target の反復 slot に `Choice` と `none` が届く不一致 |
| 67-p2-target-specialization-cons | P2 capability/target separation | target を list へ特殊化した `none` matcher で cons を拒否 |
| 68-p2-unseen-observable-parameter | P2 D1 finalization | `None` 型の節だけでは observable parameter を確定できない |
| 69-p2-exact-clause-mismatch | P2 D1 exact merge | 同じ result slot へ異なる clause capability が届く |
| 70-p2-recursive-annotation-is-not-evidence | P2 D4 | 再帰需要と結果注釈だけで Shape evidence を生成しない |
| 71-p2-recursive-transform-requires-flow | P2 D4 fail-closed boundary | producer/path 方程式が必要な application 経由の再帰 flow を拒否 |
| 72-p2-capability-unknown-former | P2 capability name elaboration | 未宣言 head を `Capability kind error` で拒否 |
| 73-p2-capability-arity-mismatch | P2 capability kind elaboration | user inductive head の arity 不一致を `Capability kind error` で拒否 |
| 74-p2-capability-alias-head | P2 canonical former discipline | transparent surface type alias の capability head 使用を `Capability kind error` で拒否 |
| 75-p2-local-capability-unknown-former | P2 local annotation elaboration | 式内注釈からの未宣言 capability head bypass を `Capability kind error` で拒否 |
| 76-p2-recursive-alias-hiding | P2 D4 scoped producer flow | local alias 経由で recursive producer origin を隠す next matcher を拒否 |
| 77-p2-forward-producer-is-unseen | P2 D4 batch producer flow | signature prepass で型だけ見える未確定 forward producer を Known evidence にせず拒否 |
| 78-p2-forward-alias-cycle | P2 D4 cross-definition flow | 先行 alias から現在の producer へ戻る top-level cycle を Known evidence にせず拒否 |
| 79-p2-recursive-closure-hiding | P2 D4 scoped closure flow | recursive producer を capture した local closure の適用を Known evidence にせず拒否 |
| 80-p2-capability-builtin-collision | P2 frozen former signature | builtin と同じ canonical ID を持つ user inductive を `Capability kind error` で拒否 |
| 81-p2-completed-alias-cycle | P2 D4 closed producer SCC | 完了済み alias-only cycle を Known evidence にせず拒否 |
| 82-p2-any-cannot-witness-capability | P2 match-site fail-closed | `Any` を structured matcher capability の witness にせず拒否 |
| 83-p2-ordinary-annotation-rigidity | P2 D3 annotation checking | `forall a. a -> a` の通常型 binder を rigid にし，定数を返す過剰一般注釈を拒否 |
| 84-p2-nested-annotation-rigidity | P2 D3 nested annotation checking | top-level の rigid 通常型 binder を nested annotation でも共有し，特殊化を拒否 |
| 85-p2-pattern-function-annotation-rigidity | P2 D3 pattern-function checking | pattern function の明示通常型 binder を rigid に検査する |
| 86-p2-pattern-function-nested-annotation-rigidity | P2 D3 nested pattern annotation | pattern-function body の nested annotation でも同じ rigid binder を共有する |
| 87-p2-capability-annotation-rigidity | P2 D3 capability annotation checking | `Matcher p Integer` の capability binder を rigid にし，`something` による `none` への特殊化を拒否 |
| 88-patfun-param-target | PATFUN-DEF / PAT-EMBED target | `~parameter` が宣言された `Integer` target を保持し，結果の `Bool` へ再型付けされることを拒否 |
| 89-patfun-exact-arity | PAT-APP exact arity | target-only の関数型統一では受理できる pattern function の過少適用を拒否 |
| 90-closed-field-slot-application | PP-Con / Matcher Consistency (1a) | closed list field が推論した `MatcherSlot` へ `something` を渡す適用を拒否 |

## ケース追加時の注意

- 1ファイル1ケース。先頭コメントに対応規則と期待エラーを書く。
- 追加時は必ず実行して、**意図したエラーで**拒否されることを確認する
  (無関係なエラーや parse error で偶然 reject されると回帰検出にならない)。
- 72--75 と 80 は共通の `Type error:` に加えて `Capability kind error` を含むことを
  確認する。これにより，後段の型推論エラーによる偶然の reject と区別する。
- 76--79 と 81 は `producer/path Shape equations` を含むことを確認し、別の型エラーで
  偶然 reject されていないことを確認する。
- 82 は `Any cannot witness a structured matcher capability` を含むことを確認する。
- 83--86 は `TSkolem`，87 は capability skolem の surface 表示 `Matcher $skc` を
  含むことを確認し，別の型エラーで偶然 reject されていないことを確認する。
- 88 は `Integer` と `Bool`，89 は `expects 2 arguments, but got 1` を含むことを
  確認し，pattern function の別の検査で偶然 reject されていないことを確認する。
- 90 は `MatcherSlot` を含むことを確認し，closed field の定義自体ではなく，推論された
  consumer slot への不適合な適用で reject されたことを確認する。
- 受理側の対になるケースがあれば `mini-test/` に置く
  (例: `mini-test/120-patfun-struct-index.egi`)。
