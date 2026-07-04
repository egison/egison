# 拡張可能 CAS タワー実装手順書

[type-cas-tower.md](./type-cas-tower.md) (D1–D5 全決定済み) と [type-cas-quotient.md](./type-cas-quotient.md) を実装に落とすための手順書。検収基準は [cas-tower-usecases/](./cas-tower-usecases/) の 8 本。

設計指針 (tower.md §8): **理論をできるだけ単純に** — 実装でも、迷ったらメタ理論が単純になる方・コードが減る方に倒す。

最終更新: 2026-07-04 (初版)

---

## 0. 決定事項の実装への写像

| 決定 | 実装上の帰結 |
|---|---|
| D3 (透明エイリアスのみ) | nominal 型機構は**タワーには不要**。unifier への追加は「alias 展開」だけ |
| D5 (`where embed` 廃止) | embed 実行系・dispatch table・菱形列挙器・宣言時 PBT が**すべて不要**。runtime の昇格は既存 `casReshapeAs` 一本 |
| D2 (型スコープ規則なし) | 規則機構 (`declare rule` / `iterateRulesCAS` / trigger filter) は**一切変更しない** |
| D1 (単一順序の一意 LUB) | 新規実装の中心 = **順序の動的化 + join + 半束性検査** (型レベルのみ) |
| D4 (商はタワー外) | 商機構 q1–q4 は**独立した実装線** (§5)。タワー側は何も背負わない |

**重要な帰結**: tower.md §4 の当初見積 (γ 大・δ 中) は D2/D5 で大幅に縮小した。旧 γ (CASCanonical class + primCASNormalizeAs + instance 群) と旧 δ (coerce の dispatch 化) は、canonicalize = 構造選択 = **既存の `casReshapeAs`**、coerce = **既存の注釈 reshape** に潰れたため、ほぼ消滅している。runtime 側の新規コードは商機構を除きゼロに近く、**実装の重心は型レベル (parser / prepass / unifier / join)** にある。

---

## 1. S0: 事前スパイク — 現状能力の棚卸し (半日)

実装前に、既存実装がどこまで動くかを mini-test で確認する。結果次第で β/γ′ の作業量が変わる。

1. **nested reshape の現状**: alias なしの生の型で usecase 04 相当を書き、atom 分離が動くか:
   ```egison
   def p : Poly (Poly Integer [i]) [x] := (2 + 3*i) + (1 - i)*x + 4*x^2
   ```
   設計上は実装済みのはず (type-cas.md §reshape の `Poly (Poly Integer [z]) [..]` 例)。`Frac (Poly Integer [i])` 係数などの深い入れ子も試す
2. **吸収律の現状**: `casReshapeAs C (casReshapeAs B v) = casReshapeAs C v` をいくつかの (B, C, v) で確認 (D5 の要検証項目)
3. **混在表現の演算**: nested 注釈済みの値と flat の値を `+` した結果を確認。設計上は「演算出口は default canonical form に落ち、注釈で target form に戻す」(tower 不変条件 + trust-the-annotation)。この想定どおりかを検証
4. 結果を本書 §7 (リスク) に追記する

### S0 結果 (2026-07-04 実施。mini-test/125-cas-s0-spike.egi に現状をピン留め済み)

1. **nested reshape (flat → nested): 動作する** — 規則付き原子 (`i`, `i^2 = -1`) 込みで atom 分離・観察型 (`Poly (Poly Integer [i]) [x]`) とも正しい (mini-test 107 §1.4 の既存確認とも整合)
2. **flatten (nested → flat): 未実装** — `def v : Poly Integer [i,x] := <nested値>` が no-op で nested 表現のまま残る。**吸収律はこの方向で破れている** → γ′ の主作業
3. **混在表現演算: nested 形が伝播** (左オペランドの形が残る)。設計の「演算出口はデフォルト形式」と乖離。さらに深刻な帰結として、**nested 由来と flat 由来の同類項がマージされない** — `i + (-i)` が 0 にならず、表現をまたぐ意味論的等価 (x − y = 0) が現状壊れている (mini-test 107 の範囲では顕在化していなかった既存問題)。対処は §4
4. 性能ベースライン (本マシン): riemann S2 ≈ 0.6s、cubic ≈ 0.6s — §6 参照

---

## 2. Phase α: 透明 type alias (工数: 小)

**変更ファイル**: `hs-src/Language/Egison/Parser/NonS.hs`、prepass (EnvBuilder)、`Type/Types.hs`、`Type/Unify.hs`、型 pretty printer、`Math/CAS.hs` (prettyTypeOf の表示のみ)

1. **構文**: `declare cas-type <Name> := <Type>`。declare 族に追加し、既存の型パーサを再利用。引数付き alias は不可 (MVP)
2. **prepass 登録**: `DeclareEnv` に `"cas-type" : Map String Type` を追加 (動的 Map + 型付きアクセサの既存パターン)。登録時検査:
   - 組み込み型名 (`Integer` / `Frac` / `Poly` / ...) との衝突 → エラー
   - 再宣言 → エラー
   - **多段 alias は登録時に完全展開**して格納 (`Qx := Poly Q [x]` は `Poly (Frac Integer) [x]` で保存)。循環は展開時に検出してエラー
3. **単一化**: 型検査の入口 (と注釈型の受け取り口) に `expandAlias :: DeclareEnv -> Type -> Type` を挟む。登録時に展開済みなので、参照位置での 1 段引き + 深い位置の置換だけで済む。unifier 本体のロジックは不変
4. **注釈 reshape との接続**: `IReshape T e` の `T` が alias を含む場合、展開後の型を `casReshapeAs` に渡す
5. **表示**: 逆引き表 (展開後の型 → alias 名) を持ち、型の pretty print と観察型 (`prettyTypeOf` / `inspect`) は一致する alias があれば名前を優先表示。同一型に複数 alias があれば先に宣言された方
6. **mini-test**: 宣言・注釈・多段 alias・衝突エラー・循環エラー・表示

**検収**: usecase [01](./cas-tower-usecases/01-type-alias.egi)・[02](./cas-tower-usecases/02-gaussian-integers.egi)・[05](./cas-tower-usecases/05-quadratic-extension.egi) が通る (02/05 は `substitute` ベースの conj を含む — substitute は既存関数)。

### α 実施結果 (2026-07-04 実装完了)

- **実装**: AST に `DeclareCasType`、parser に `declare cas-type N := T` (行境界つき型パーサ `typeExprIndented` を新設 — 既存 `typeExpr` は貪欲で、宣言末尾の型が次行の宣言を型原子として飲み込むため、インデントガードで打ち切る)。EnvBuilder が prepass で収集・検証 (大文字必須 / 組み込み・inductive・重複との衝突エラー) し、**固定点解決**で alias-in-alias を展開 (前方参照 OK = 他の declare 族と同じ宣言順不問、循環は名前つきエラー)。展開は `typeExprToType` の全 seam (EnvBuilder 全分岐 + Desugar の instance 名生成 / PatternFunctionDecl / DeclareSymbol / 式注釈 IReshape) に適用。`EvalState.casTypeAliasEnv` でロードバッチを跨いで永続化
- **検証**: mini-test 126 (def/式注釈・多段 alias・前方参照・規則付き原子 GaussianInt の演算・substitute conj/norm) 全 pass。エラーパス 4 種 (重複・組み込み衝突・循環・小文字名) 確認済み。回帰: 代表 7 sample エラーゼロ (全て 1s 未満)・mini-test **89/89**・cabal test **21/21**
- **意図的に後回し**: 表示側の alias 逆引き (pretty printer / 観察型で `Qx` と表示する nice-to-have)。実装は展開後の構造型を表示する。必要になったら逆引き表 (展開型 → 宣言名、宣言順優先) を Type/Pretty と prettyTypeOf に足す

---

## 3. Phase β: cas-subtype + 動的 isSubtype + join (D1) (工数: 中)

**変更ファイル**: `NonS.hs`、prepass、`Type/Subtype.hs` (新規)、`Type/Join.hs`、elaboration (`Type/Check.hs`)

1. **構文**: `declare cas-subtype <Type> ⊂ <Type>` — 関係のみ (D5)。`⊂` の ASCII 代替 (`<:` 等) を用意するかはパーサ着手時に決める (§7)
2. **prepass 登録**: alias 展開後の型 (スキーム) 対を `DeclareEnv "cas-subtype"` に登録
3. **`Type/Subtype.hs` (新規)**:
   - 現在 `Type/Join.hs` に hard-code されている骨格規則 (5 段タワー + 原子集合包含 + 係数伝播) を導出規則としてデータ化
   - 宣言辺 + 骨格規則の**推移閉包**。宣言ノードは有限個、スキームは構造再帰 (部分型判定は「宣言辺の graph 到達 + 骨格規則の構造分解」の交互適用。構造規則は型サイズを減らすので停止)
   - `isSubtype :: DeclareEnv -> Type -> Type -> Bool` を提供し、既存の hard-code 呼び出しを置換
4. **join の動的化**: 極小上界の列挙 → 一意ならそれ、非一意は内部エラー (D1 の検査が防いでいるはず)。`MathValue` が常に top
5. **D1 半束性検査 (declare 時)**: 新辺 `A ⊂ N` の追加時、
   - N の下錐にある既存型ペア (a, b) について、既存 join J(a,b) が N の下に導出できるかを検査 (**下錐の join 閉性**)
   - 破れたら: エラー + 欠落辺 `declare cas-subtype J ⊂ N` の**提案をメッセージに埋め込む**
   - 新 join が旧 join より真に小さくなるペアがあれば: **warning** (細化単調性 — 値は不変、静的型のみ精密化)
   - 検査対象は「宣言ノード + 骨格の代表ノード」の有限集合で開始し、原子集合はスキームの側条件として扱う (取りこぼしが見つかったら精密化 — §7)
6. **冗長辺**: 骨格 + alias 展開から導出可能な辺 (例: `Integer ⊂ GaussianInt`) は no-op として受理し、その旨 warning (usecase 03 の implementation note の検証を兼ねる)
7. **elaboration**: 二項演算で join 型が operand と異なるとき、現状の runtime 内部昇格 (casPlus 等の混在レベル処理) で足りるか、明示 `IReshape` 挿入が要るか — S0 の結果で確定。原則: **値の正しさは runtime 昇格で既に保たれている**ので、挿入が要るのは「join 型の canonical form で保持したい」場合のみ

**検収**: usecase [03](./cas-tower-usecases/03-subtype-promotion.egi)・[06](./cas-tower-usecases/06-combined-extensions.egi)・[08](./cas-tower-usecases/08-join-completion.egi) (08 は半束性検査のエラーメッセージ・完備化提案・細化 warning まで含む)。

### β 実施結果 (2026-07-04 実装完了)

- **実装**: AST `DeclareCasSubtype` / parser `declare cas-subtype A ⊂ B` (ASCII 代替 `<:` 両対応) / **`Type/Subtype.hs` 新設**:
  1. `skeletonSubtype` — 設計の包含表を係数再帰まで完全化 (nested ↔ flat は**意図的に無関係** = 宣言辺の存在理由)
  2. `skeletonJoin` — 設計 join 表どおり (**level 2 ⊔ level 3 = level 4 を実装**。旧 `Type/Join.joinTypes` は level 5 を返す設計不一致があるが、呼び出し元ゼロのため温存 — §7)
  3. `isSubtypeWith` / `joinTypesWith` — 宣言辺込みの順序 (worklist 閉包 + 極小上界の一意性)
  4. `checkEdgeAddition` — D1 検査 5 分類 (OK / 冗長 / 循環 / 曖昧 + 完備化提案 / 細化)
- EnvBuilder が prepass で辺を収集し宣言順に D1 検査: 冗長 → warning + **保存** (端点を後続検査のノード集合に残す)、循環・曖昧 (提案つき)・非 CAS → エラー、細化 → witness つき warning。`EvalState.casSubtypeEdges` でバッチ跨ぎ永続
- **検証**: mini-test 127 (EdgeOk 2 種・冗長 warning・評価非干渉)。誤りパス 5 種を確認 — 曖昧性エラーは usecase 08 どおり**数学的に真の完備化辺を提案** (`Poly Integer [i, x] <: Poly (Poly Integer [i]) [x]`)、完備化後は受理 (冗長化)、細化 warning は witness (`join(Poly Integer [i], Poly Integer [x]): Poly Integer [i, x] -> Poly (Poly Integer [i]) [x]`) つき。回帰: 代表 7 sample エラーゼロ・mini-test 90/90・cabal test PASS
- **既知の近似**: ペア列挙は宣言ノード集合上の有限近似 — 骨格のみで target の下に入る型は、どこかでノード宣言 (冗長辺でよい) されていなければ検査対象外。反例駆動で精密化 (§7)
- **未接続 (意図的)**: instance 解決 (`Type/Join.isSubtype`) と型推論への動的順序の配線は dispatch への影響評価が必要なため保留 (§7)。現状の辺の意味論的効果は宣言時検証のみ — これは D1 の設計どおり (join は現状 static footprint を持たない)

---

## 4. Phase γ′ (縮小版): canonicalize 経路の整備 (工数: 小〜中)

旧 γ/δ の残骸。**新しい機構は作らない** — 既存 `casReshapeAs` の網羅性と法則を固める。S0 の結果、作業は次の 3 点に具体化された:

1. **flatten (nested → flat) の実装** (S0-2 の穴): `casReshapeAs` に「係数位置の `CASPoly` を外側モノミアルへ展開する」demote 方向を追加する。atom 分離 (flat → nested) は実装済みなので、これが吸収律の欠けている半分
2. **演算出口のデフォルト形式化** (S0-3/S0-4 の穴): `casNormalize` (または算術 op の出口) で nested 係数を検出したら flatten する。これにより
   - 混在表現の同類項マージ不全 (`i + (-i) ≠ 0`) が解消し、表現をまたぐ意味論的等価が回復する
   - 「**nested 形は reshape 直後にのみ存在し、演算を通すとデフォルト (flat) 形式に落ちる**」という設計どおりの不変条件が立つ (usecase 04 は演算結果を再注釈して nested に戻すパターンなので影響なし)
   - 性能: 係数が `CASPoly` かの検査だけなので、通常経路 (flat のみ) はゼロコスト
3. **吸収律の恒常テスト**: サンプル battery (`0, 1, -1, 2, i, x, x+1, (1+i)*x^2`) × 対象型ペアの mini-test (+ 可能なら `cabal test` に QuickCheck property)。**mini-test 125 の GAP ピンを新挙動に更新**するのを忘れない

**検収**: usecase [04](./cas-tower-usecases/04-gaussian-poly.egi) (中核ケーススタディ)。**ここまででタワー側は完成**。

### γ′ 実施結果 (2026-07-04 実装完了) — **M3: タワー完成**

- **flatten の実装**: `casNormalizePoly` を `casNormalizePolyWith flatten` に一般化。flatten モード (全算術経路のデフォルト) では nested 係数 (`CASPoly` 係数、`CASFrac (CASPoly _) (CASInteger _)` 係数) を `flattenNestedTerm` が外側モノミアルへ分配展開。ガードは係数コンストラクタ検査のみで、nested が現れない通常経路は実質ゼロコスト
- **reshape 側は keep-nested モード**: `reshapeAsPoly` の最終グルーピングのみ `casNormalizePolyWith False` (係数の再正規化も抑止 — 多段 nested の保護)。**reshape 入口の `casNormalize` が入力を flat に正規化するため reshape は「値のみの関数」になり、吸収律 `casReshapeAs C . casReshapeAs B = casReshapeAs C` が構成から成立** (D5 coherence の実装対応物)
- **S0 の 3 ギャップすべて解消** (mini-test 125 を fixed-behavior ピンに全面更新): flatten reshape / 演算出口のデフォルト flat 形式 / 表現またぎの項マージ (`i + (-i) = 0` が全表現組合せで成立)
- **吸収律バッテリ = mini-test 128**: nested→flat・flat→nested・**cross-nesting (主変数の付け替え)**・round-trip (usecase 04 の p_back パターン)・微分+再注釈、を構造的等価+show 等価で検証
- **付随修正 2 件**:
  1. `Infer.hs` IReshape に **representation-directive 寛容化** — CAS 型間の注釈重ね掛け (`((v : nested) : flat)` 等) を型エラーにしない (trust-the-annotation の型検査側対応。非 CAS の不整合は従来どおりエラー)
  2. `Pretty.hs` の IExpr instance の `IReshape` ケース欠落 (pre-existing — 型エラー表示時に non-exhaustive でクラッシュ) を補修
- **発見してピン留めした既存挙動**: `def x := e` (注釈なし) は TypedDesugar の define-site reshape により**推論スキームへ再整形**され、推論は左オペランドの型を採る — `def m := p + pFlat` は束縛時に nested へ戻る (演算出口自体は flat。値保存であり設計と整合。125 S0-3b/3c にピン)
- **回帰**: 代表 7 sample エラーゼロ・**性能劣化なし** (riemann S2 572ms / cubic 575ms — ベースライン同等)・mini-test 91/91・cabal test PASS

---

## 5. 商機構 q1–q4 (独立線、工数: 中)

タワーと独立に進められる (α のパーサ基盤にだけ乗る)。詳細設計は [type-cas-quotient.md](./type-cas-quotient.md)。

1. **q1 nominal 型導入**: `Type` に商型コンストラクタ (`TQuotient String` 等) を追加。unifier は同名同士のみ単一化。**⊂ 関係はどこにも張らない** (`MathValue` とも)。`Type/TypeClassResolve.hs` の sibling fallback (親型 instance への委譲) を商型では**遮断** — `Eq MathValue` に落ちると等価性が壊れるため (quotient.md §5)
2. **q2 構文 + 登録**: `declare cas-quotient <Name> := <Type> by <expr>`。prepass で `DeclareEnv "quotient" : Map String (Type, reduce closure)` に登録。reduce の closure 評価タイミングは `preBindDeclaredSymbols` と同様に Phase 8 (`recursiveBindAll`) 前後の順序に注意。
   - **未決の確定**: 注釈による暗黙 proj (`def a : Mod7 := 5`) を**許す** (推奨) — elaboration が商型注釈に `IProj` (reshape の商版) を挿入。usecase 07 の open question をここで閉じる
3. **q3 instance 自動導出**: declare 時に準同型 instance (`AddMonoid`/`MulMonoid`/`Ring` 等: `(+) a b := proj (repr a + repr b)`) と `Eq` (`reduce (repr a - repr b) = 0`) を `instanceEnv` に機械生成で登録。`proj`/`repr` primitive を追加。パターン2 演算 (`inv`/`gcd`/比較) はユーザ定義に委ね、base への自動委譲はしない
4. **q4 合同律検査**: declare 時にサンプル battery で冪等 (`reduce . reduce = reduce`) と合同 (`reduce (x∘y) = reduce (reduce x ∘ reduce y)`, ∘ = +, *) を検査。失敗はエラー、opt-out フラグ付き

**検収**: usecase [07](./cas-tower-usecases/07-modular.egi) (混在演算が型エラーになることのテスト含む)。q5 (`Poly 商型 atoms`) はスコープ外 (optional)。

### M4 実施結果 (2026-07-04 実装完了) — **全マイルストーン達成**

- **実装形態は「素朴なマクロ」**: `declare cas-quotient Q := T by f` は評価パイプライン冒頭 (`Eval.expandCasQuotientDecls`、環境構築より前) で通常の TopExpr 列に展開 — `def reduceQ := f` (ユーザ AST 直挿し) + `projQ`/`reprQ` + 準同型 instance 群 (`Eq`/`AddSemigroup`〜`Ring`) + **q4 の合同律 assertion 3 本** (冪等・`+'`/`*'` 合同、整数バッテリ)。生成部はテンプレート文字列を既存パーサで再パース。新機構は unsafe cast primitive `casQuotientCast : ∀ a b. a → b` (生成コード専用) の 1 個だけ
- **q1 (nominal) は alias 環境への登録で獲得**: `Q → TInductive Q []` を cas-type alias 環境に入れるだけで、全注釈 seam が名前を不透明型に写す。同名同士しか単一化せず、`declare cas-subtype` は非 CAS として拒否 (D4 の順序不参加が自動成立)。EvalState の新フィールドはゼロ
- **横断は明示で確定** (usecase 07 の open question): `projQ` / `reprQ`。注釈による暗黙 proj は将来の糖衣候補
- **混在演算** (`Mod7 + Integer`): instance 不一致で dispatch 不成立 — gradual では Warning + 未解決シンボリック値として可視的に失敗、strict モードでは型エラー
- **検証** (mini-test 129): 準同型演算 (per-op reduce、代表元 0..6 維持)・型 dispatch Eq (`12 == 5 in Mod7` = True)・repr・パターン2 演算 (Fermat 逆元)・複数商の共存 (Mod7/Mod5)・宣言時合同律検査、すべて動作。回帰: 代表 sample エラーゼロ性能不変・mini-test 92/92・cabal test PASS

---

## 6. テスト・回帰手順

- mini-test は既存規約どおり番号 prefix (`NN-cas-alias.egi` 等、現状の最大番号に続ける)。フェーズごとに追加
- **各 Phase 完了時に必ず** egison/CLAUDE.md の標準検査を実施: 代表 sample 7 本、mini-test 全件ループ、`cabal test` + warning/error grep
- **性能ガード (本マシン、2026-07-04 計測)**: riemann S2 ≈ **0.6s**、cubic ≈ **0.6s**。CLAUDE.md の参考値 (~5s / ~4s) は旧マシンのもので約 8 倍遅い上限として読む。本マシンで代表 sample が **1s を超えたら要調査**
- α/β は型レベルのみの変更なので数式処理の性能は原則不変のはず。slowdown が出たら unifier の alias 展開・isSubtype 呼び出しのキャッシュ (`DeclareEnv` 参照の memo 化) を疑う
- コード内コメントは英語 (CLAUDE.md)。`cabal` 使用・`gtimeout` 必須・自動 commit 禁止も同規約どおり

---

## 7. リスクと未知 (S0 で更新する)

| 項目 | 内容 | 対処 |
|---|---|---|
| 混在表現の演算 | ~~S0 で確認~~ → **確定**: 出口は default form にならず nested が伝播、表現またぎの項マージも不全 | γ′-2 (演算出口のデフォルト形式化) で解消する。既存挙動のピンは mini-test 125 |
| スキームレベル半束性検査 | 原子集合の側条件込みの全ペア検査の実装精度 | 有限近似 (宣言ノード + 骨格代表) で開始、反例駆動で精密化 |
| `⊂` トークン | Unicode のみか ASCII 代替 (`<:`) も置くか | パーサ着手時に決定 (小) |
| alias 逆引き表示 | 同一展開型に複数 alias がある場合の表示の一意性 | 宣言順優先で固定 |
| 商型の fallback 遮断 | sibling fallback の例外化が既存 typeclass 解決に波及しないか | q1 で mini-test を先に書く |
| **`Type/Join.joinTypes` の設計不一致** (β で発見) | 旧実装は Poly ⊔ Frac → level 5 を返すが、設計は level 4。呼び出し元ゼロなので実害なし | `Type/Subtype.skeletonJoin` が設計どおりの正。呼び出し元が現れる時点で Subtype 版に一本化 |
| **isSubtype の二重化** (β の意図的判断) | instance 解決は旧 `Join.isSubtype` (部分的な骨格) のまま。完全骨格 `Subtype.skeletonSubtype` に切り替えると適用可能 instance が広がり dispatch が変わりうる | 影響評価 + 全回帰つきで別途一本化 (γ′ 以降) |
| reduce closure の束縛順序 | `declare cas-quotient` の reduce がユーザ関数を参照する場合の prepass/eval 順序 | `preBindDeclaredSymbols` の前例に倣う |

---

## 8. マイルストーン

| M | 内容 | 検収 |
|---|---|---|
| M0 | S0 スパイク完了、§7 更新 | — |
| M1 | Phase α | usecases 01・02・05 |
| M2 | Phase β (D1 検査込み) | usecases 03・06・08 |
| M3 | Phase γ′ (吸収律テスト恒常化) | usecase 04 — **タワー完成** |
| M4 | 商機構 q1–q4 | usecase 07 — **論文の実験に着手可能** |

M1→M2→M3 は直列。M4 は M1 完了後いつでも並行可 (q1 の nominal はタワーに依存しない)。

論文側の実験 (観察型の注釈分類測定、コーパス評価) は本書のスコープ外 — タワー/商機構完成後に別途計画する。

---

## 9. 改訂履歴

- 2026-07-04: 初版。D1–D5 全決定 (コミット a90b9b23) を受けて作成。旧 γ/δ が D2/D5 により縮小したことを §0 に明記し、フェーズ構成を α / β / γ′ / q1–q4 に再編。
