# Function Symbol 設計改良案 — Formurae からの要求 (2026-07-08 起草)

> **2026-07-08 実装完了**: A = `functionSymbol : String -> [MathValue] -> MathValue`
> (Primitives.hs; mini-test/147)。C = `mapSymbols` に quote 透過ケース
> (expression.egi; 再構築は新プリミティブ `quoteScalar`; mini-test/148)。
> D = `mathFunctionName`(apply1..4 の頭の名前; mini-test/149)+
> func 名の添字は `symbol $b $is` で既に構造マッチ可能と確認。
> **B は診断の結果、extendAtoms ではなくランタイムディスパッチの穴だった**:
> `runtimeDispatchCandidates` が MathValue インスタンスを候補から除外する
> (自己選択ガード)ため、let 一般化された組合せが呼び出し点で**整数 0 に
> 潰れる**(LBM の静止方向 c=(0,0,0))と runtime type TInt がどの候補にも
> 合致せず MulSemigroup 事故になっていた。修正 = Core.hs IRuntimeDispatch
> に **CAS 値なら MathValue 辞書へのフォールバック**を追加(インスタンス
> 内部の呼び出し点は TMathValue 型でディスパッチ節が出ないためループ不能;
> mini-test/150)。単純な let×関数シンボル×除算は既に先行修正
> (let 一般化改修)で通っており、残っていたのはこの整数崩壊ケースのみ。
> **受け入れ**: formurae の LBM を functionSymbol 族(38 defs → map 2行)+
> feq の let 復活で再生成し **.fmr バイト一致**・チェック green。

現行仕様は [function-symbol.md](function-symbol.md)(2026-07-07 位置正準形で確定)。
本文書は、その仕様を**最大のヘビーユーザである Formurae**
(`PL/formurae`: .fe → fec → 埋め込み Egison → CAS 展開 → Formura)の実運用に
突き合わせ、設計改良の候補を優先度つきで整理する。**提案であり仕様ではない。**

## 0. Formurae が関数シンボルに課している役割

- 格子場 = 座標記号上の抽象関数 `def u := function (x, y, z)`。
- 格子参照 = `substitute` による座標シフトが生む未解釈適用 `u (x+hx) y z`。
- プリンタ = `func $g $args` へのパターンマッチ+オフセット厳密復元 `(arg−x)/h`。
- 添字つき関数族 = `def E_i := generateTensor (\[i] -> function (x,y,z)) [3]`
  (定義文脈が名前と添字を捕捉)。

この設計自体は成功している(23例・全て実測検証済み)。以下は「うまくいって
いるがハックで支えている」箇所と「これ以上進めない」箇所の列挙。

## 1. 現状の摩擦点(証拠つき)

### F1. 名前をプログラムで作れない(最重要)

`IFunctionExpr` は環境の定義文脈からしか名前を取れない
(`Core.hs:383-397`: `Env _ Nothing _` → エラー、添字は generateTensor 経由のみ)。
その帰結:

- `examples/lbm_d3q19/lbm_d3q19.egi`: **`def fN := function (x,y,z)` を19本手書き**
  (方向表への map で57本の式は生成できるのに、場の宣言だけが map できない)。
- `examples/mhd_ot/mhd_ot.egi`: 中間流束場 `fmx1..` 等を**1本ずつ def**。
- fec も local 場ごとに `def w := function (x, y, z)` を1行ずつ**emit する**しかない。
- .fe ロードマップの **`field f : family 19`(LBM の表層化)がここでブロック**
  されている(formurae/DSL-DESIGN.md の残項目)。

### F2. 名前・添字の構造的アクセスがない

- fmrgen の `fmrFieldName` は `show g` の文字列を `"_"` で split して
  E_1 → Ex を再構成(fmrgen.egi:249)。名前 Symbol は内部では
  `Symbol "" "E" [Sub 1]` と**構造を持っている**のに、matcher `func $name $args`
  から先は show 経由でしか分解できない。
- もっと悪い例: mathfunc 適用の頭の名前取り出しが
  `def applyName g := nth 2 (S.split "\"" (show g))`(fmrgen.egi:282)—
  `#<lambda Var "cos" [] …>` という show 文字列を引用符で split している。

### F3. substitute が quote に入れない(計量機構のホットパス)

`mapSymbols` の quote 非対応(クラッシュ)+ `expandAll` が apply1 を再 quote
するため、fmrgen は再帰 `unquoteAll`(fmrgen.egi:165-183)を自前で持つ。
embedding → 計量 → 半セル substitute の経路で毎回必要。
(隣接課題: `sqrt` の多項式完全平方 radicand 未簡約 = 一般計量 √det g の穴、
チップ発行済み。)

### F4. 型レベル原子集合の閉鎖 × let 一般化

関数適用は型レベル原子集合(`SymbolSetClosed [TypeAtom]`, Types.hs:106)に
入れない現行制限(function-symbol.md「必要になれば extendAtoms 経路で開放」)
のもとで、関数本体内の数式 let が
`no MulSemigroup instance for TTerm TInt (SymbolSetClosed …)` で落ちる
(LBM の feq で発症)。**fec の `let` = インライン展開は、この制限を回避する
ために焼き込まれた設計**であり、生成 .egi の膨張と誤差箇所の分散を招いている。

### F5. 配置メタデータが常にサイドカー

スタガード場 = `(f, σ)` タプル、微分形式 = `(complex, degree, comps)` タプル
(fmrgen.egi:100-213)。関数シンボル自体は自分の配置を知らないため、
`yeeRef`/`dForm` は σ 表の zip 配管を持ち歩く。動いてはいる(バイト一致で
検証済み)が、fec 側にも同じ配置表が二重に存在する。

### 摩擦でない点(確認済み)

- **位置正準形の導関数索引(392a3f29)は Formurae の将来要求と整合**:
  一般計量の Christoffel 記号 Γ^k_ij = ½ g^kl(∂g…) を chart の関数シンボル
  微分で作る計画にとって、Schwarz ソート済み `f|1|2` はまさに欲しい形。
- 引数の値ベース等値・オフセット復元 `(e−x)/h` は現設計のままで十分。

## 2. 改良提案(優先度順)

### A. 第一級の名前つき構築子(F1 の根治)

```egison
-- 案: プリミティブ追加
functionSymbol : String -> [MathValue] -> MathValue
-- 例
def fs : [MathValue] :=
  map (\n -> functionSymbol (S.append "f" (show n)) [x, y, z]) (between 0 18)
```

- 実装は小さい: `updateFunctionArgs`(Primitives.hs:184、args 差し替えは既存)
  と同型で、name Symbol を文字列から作るだけ。定義文脈経由の現行 `function`
  はそのまま残す(後方互換)。
- 添字つき版も自然に定まる: `functionSymbolI : String -> [Index] -> [MathValue] -> MathValue`
  か、名前文字列 "E_1" を Symbol+Sub に正規化するかは要決定
  (F2 とセットで決めるのがよい)。
- **効果**: LBM/MHD の def 羅列消滅、fec の local emit 簡素化、
  `.fe` の `field f : family 19` が実装可能に。

### B. 型レベル原子集合の開放(F4; 設計済みの「必要時」が来た)

function-symbol.md が予告した compound atom と同じ `extendAtoms` 経路で
`Poly … [f x]` を許す。**受け入れ条件 = LBM の feq を let で書いて型が付く**
こと。効果は fec の `let` を本物の let にできること(生成 .egi の縮小・
エラー位置の改善)。リスクは推論コスト — mini-test + 代表サンプルの
slowdown 検査(CLAUDE.md の手順)で計測してから採否を決める。

### C. substitute の quote 透過(F3)

`mapSymbols` の quote ケースを「中を再帰し、変化がなければ quote を保つ/
変化があれば unquote して置換結果を返す(または再 quote)」に定義する。
fmrgen の `unquoteAll` 層を削除でき、一般計量ロードマップ
(√det g、チップ済みの sqrt 穴と合流)の前提が整う。
再 quote の要否は CAS 正規化の性能特性(quote はもともと因子の原子化が目的)
と相談: **提案は「substitute は quote を破らず中だけ書き換える」**。

### D. 名前・添字・mathfunc 頭の構造的アクセサ(F2)

- matcher 拡張: `func (symbol $base $indices) $args` で名前の添字リストが
  取れることを保証(現状も内部は Symbol なので、実質はドキュメント+
  テストの整備で足りる可能性が高い — 要確認)。
- mathfunc 適用の頭: `applyName : MathValue -> String` 相当のプリミティブ
  (または `apply1 (mathfunc $name) $arg` パターン)を提供し、
  show-split ハックを廃止。

### E. (要議論)配置メタデータの搭載(F5)

関数シンボルに任意属性(例: σ ∈ {0,½}³、complex/degree)を持たせる案。
**ただし推奨は保留**: 等値性(σ は等値に効くのか)、show、substitute との
相互作用が複雑化する一方、現行のタプル+σ 表は透明でバイト一致検証とも
相性がよい。A〜D が入った後に、fec 側の配置表二重管理がまだ痛いかで再評価。

## 3. 採用順の提案

1. **A**(小さく・効果最大・後方互換)→ 直後に formurae 側で LBM を family 化
   して実地検証(.fmr バイト一致 or 同値+チェック green が受け入れ条件)。
2. **D**(A の添字設計と同時に決めるのが安い)。
3. **C**(計量まわりの次の一手=一般計量の前提)。
4. **B**(計測してから; fec の let 設計を書き換える価値があるかで判断)。
5. **E** は A〜D 後に再評価。

## 4. 検収の考え方

どの案も **Formurae の回帰(`make all` 29 checks + .fmr バイト一致)を
egison 側変更の受け入れテストとして使える**。egison 単体では
mini-test に「functionSymbol で作った族の substitute/微分/等値」
「let 内数式の型付け(B)」「quote 内 substitute(C)」を追加する。
