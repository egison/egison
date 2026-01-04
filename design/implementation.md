TopExpr → expandLoads → Desugar (ここでデータコンストラクタとクラス情報を集める？) → 型チェック → TypedTopExpr → TypedDesugar (ここでType classとtensorMapの展開をする) → TITopExpr → 評価

やること

トップレベル定義とletrec式の定義に全て型注釈をつける。
型注釈をつける際にスカラー仮引数とテンソル仮引数の注釈を消す。
inverted scalar parameterの扱いを考える。
interpreterが管理するStateに変数環境、型環境、クラス情報、データコンストラクタ情報を管理する。