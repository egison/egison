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
| 03-nested-ctor-element | PAT-CON の構造伝播(B.2.3) | `multiset something` × ネスト構築子 `num` |
| 04-something-tuple-pattern | MS Progress のタプルケース | `something` × タプルパターン(積型頭の slot) |
| 05-matcher-target-mismatch | ターゲット不一致(B.2.1) | `num` パターン × `[Integer]` |
| 06-target-type-mismatch | T-MATCHALL のターゲット側 | `matchAll 5 as multiset integer` |
| 10-patfun-body-structural | PAT-APP 構造側・本体(レビュー反例 M1) | `pair $x []` × `something` |
| 11-patfun-arg-structural | PAT-APP 構造側・引数(M1) | `idp ($x :: $xs)` × `something` |
| 12-patfun-nested-arg-structural | PAT-APP 構造側・ネスト引数(B.2.3) | `pair (num $n) []` × `multiset something` |
| 13-patfun-target | PAT-APP ターゲット側 | `seqp`(`[Tile]`)× `[Integer]` ターゲット |
| 14-patfun-arg-target | PAT-APP 引数ターゲット(B.1.2) | `seqp #1 _`(`Integer` vs `Tile`) |
| 20-patfun-linearity-unused | PATFUN-DEF 線形性(M2) | 未使用パラメータ |
| 21-patfun-linearity-order | 同 | 宣言順違反 |
| 22-patfun-linearity-dup | 同 | 重複使用 |
| 23-patfun-linearity-under-or | 同 | or 分岐配下での使用 |
| 30-value-pattern-expr-type | PAT-VALUE(B.1.1) | 値パターン内式の型エラー(`x ++ [1]`) |
| 31-nonlinear-target-type | 非線形パターン(B.1.1) | `$x :: #x`(要素 vs リスト) |
| 32-or-pattern-bindings | PAT-OR | 分岐間の束縛変数不一致 |
| 40-matcher-next-structural | Def 4.2(1a) / PP-Con(B の `weird`) | 構築子頭 hole への `something` |
| 41-matcher-body-matchsite | 本体内 match-site 検査 | matcher 本体内の `integer` × cons |
| 42-tuple-pattern-arity | PAT-TUPLE | タプルパターンの arity 不一致 |
| 50-matcher-collection-hetero | Matcher rigidity | `[something, list integer]`(異種 matcher のコレクション) |
| 51-matcher-cast-structured | Matcher rigidity | `def m2 : Matcher [Integer] := something`(構造型への束縛) |
| 52-missing-signature-constraint | シグネチャ完全性(残存制約検査) | 本体が `<=`({Ord a})を要求するのにシグネチャに無い |
| 53-matcher-alias-specialize | Matcher rigidity | `def myint : Matcher Integer := eq`(注釈による特殊化) |
| 54-something-structured-hole | PP-Con 遅延判定 | 注釈で後から [Integer] に確定する hole への `something` |

## ケース追加時の注意

- 1ファイル1ケース。先頭コメントに対応規則と期待エラーを書く。
- 追加時は必ず実行して、**意図したエラーで**拒否されることを確認する
  (無関係なエラーや parse error で偶然 reject されると回帰検出にならない)。
- 受理側の対になるケースがあれば `mini-test/` に置く
  (例: `mini-test/120-patfun-struct-index.egi`)。
