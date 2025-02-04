# AI2025実験分析

社会的選好と選択行動の分析結果

## リポジトリ構造

```
AI2025_R/
├── README.md                          # このファイル
├── .gitignore                         # Git除外設定
├── analysis/                          # 分析ファイル
│   ├── altruism_analysis/            # 利他性要因分析
│   ├── choice_analysis/              # 条件間の比の検定
│   ├── payoff_avg_analysis/          # 条件間の平均の差の分析
│   ├── payoff_preference_analysis/   # 選好の自己評価と実際の行動の整合性分析
│   ├── socialPreference_analysis/    # 社会的選好分析
│   └── time_analysis/                # 条件と所要時間の分析
├── document/                          # 分析結果
└── AI2025_data/                      # 実験データ
    ├── 20250117_3/                   # AI条件（1回目）
    ├── 20250117_4/                   # コントロール条件（1回目）
    ├── 20250120_4/                   # コントロール条件（2回目）
    └── 20250120_5/                   # AI条件（2回目）
```

# 2. 実験データ詳細　（AI→dictator_app, 基本→Base_dictator_app）

各ラウンドの決定を配列として記録：
- round_number: ラウンド番号
- choice: 選択（'X' or 'Y'）
- payoff_dictator: A役の報酬
- payoff_receiver: B役の報酬
- scenario: 選択肢の内容
  - x: [Option_X_Dictator, Option_X_Receiver]
  - y: [Option_Y_Dictator, Option_Y_Receiver]

# 3. アンケートデータ（questionnaire_app）

## 3.1 基本情報
- gender: 性別（1:男性, 2:女性, 3:その他, 4:回答しない）
- age: 年齢（18-100歳）
- gakubu: 学部（1:商学部 ～ 19:その他）
- gakunen: 学年（1:学部1年次 ～ 7:博士後期課程）
- feedback: 実験の感想（自由記述、任意）

## 3.2 AIに関する評価（各7段階評価）
- ai_satisfaction: AI満足度（1:全く満足していない ～ 7:非常に満足している）
- ai_understanding: AI理解度（1:全く理解できなかった ～ 7:十分理解できた）
- prediction_accuracy: 予測精度（1:全く一致していなかった ～ 7:完全に一致していた）
- rf_understanding: ランダムフォレスト理解（1:全く理解できなかった ～ 7:十分理解できた）
- tech_trust: テクノロジー信頼度（1:全く信頼していない ～ 7:非常に信頼している）

## 3.3 選好に関する自己評価（各7段階評価）
- selfish_preference: 利己的選好（1:全く重視しなかった ～ 7:非常に重視した）
- equality_preference: 平等選好（1:全く重視しなかった ～ 7:非常に重視した）
- efficiency_preference: 効率性選好（1:全く重視しなかった ～ 7:非常に重視した）
- competitive_preference: 競争的選好（1:全く重視しなかった ～ 7:非常に重視した）

## 3.4 報酬関連データ
- selected_dictator_round: 選ばれたディクテーターゲームのラウンド
- selected_dictator_payoff: 選ばれたディクテーターゲームの報酬
- ai_prediction_payoff: AI予測による報酬
- final_total_payoff: 最終的な合計報酬


## 分析内容

1. **条件間の比の検定** (`choice_analysis/choice_analysis.R`)
   - 使用変数：`player.choice`（X/Y選択）
   - 分析内容：
     - AI条件とコントロール条件での選択比率（特にY選択）の比較
     - コンティンジェンシー表による比率の差の検定
     - 日付別（2025-01-17, 2025-01-20）のデータを統合して分析
   - データ処理：
     - 参加者フィルタリング（`participant.visited == 1`）
     - コントロール条件の16ラウンド目を除外
     - 選択データの抽出と統合（`bind_rows`による日付別データの結合）

2. **社会的選好分析** (`socialPreference_analysis/socialPreference_analysis.R`)
   - **使用変数**:
     - 主要変数:
       - `player.payoff_dictator`, `player.payoff_receiver`（報酬額）
       - `s`（受け手有利の不平等: receiver - dictator）
       - `r`（独裁者有利の不平等: dictator - receiver）
     - 制御変数:
       - `gender`, `age`, `equality_preference`（アンケートデータ）
       - `condition`（AI/Control）
       - `round_number`（学習効果調整）

   - **分析内容**:
     1. Fehr-Schmidtモデルに基づく効用関数の最尤推定:
        ```math
        U(x) = x_D - α・max{x_R - x_D, 0} - β・max{x_D - x_R, 0}
        ```
        ここでα: 不利な不平等回避, β: 有利な不平等回避
     2. 条件間パラメータ比較（AI vs Control）:
        - 混合効果モデルでα, βの条件間差を検定
        - 尤度比検定によるモデル比較

   - **理論的意義**:
     - AI介入が不平等回避選好に与える影響の定量化
     - 実験室測定された社会的選好と宣言的選好の乖離分析
     - 文化比較研究（日本vs欧米）への知見提供

   - **分析ステップ**:
     1. データ前処理:
        - 不平等指標の絶対値変換（対数正規分布対応）
        - 選択データとアンケートデータのマージ
        - 極端値処理（中央値±3MAD）

     2. モデリング:
        ```r
        # 最尤推定のコアコード例
        utility <- function(alpha, beta, dictator, receiver) {
          inequality <- receiver - dictator
          dictator - alpha*pmax(inequality, 0) - beta*pmax(-inequality, 0)
        }
        
        # 混合効果モデル
        me_model <- lmer(alpha ~ condition + (1|participant.id), 
                       data = preference_params)
        ```

     3. 診断検証:
        - パラメータ推定の頑健性チェック（ブートストラップ法）
        - モデル適合度（AIC/BIC）
        - 残差分析（正規性・等分散性）

     4. 結果可視化:
        - 条件別α-β散布図（95%信頼楕円付き）
        - 個人別パラメータの密度プロット
        - パラメータ空間のクラスタ可視化（t-SNE法）

3. **条件間の平均の差の分析** (`payoff_avg_analysis/payoff_avg_analysis.R`)
   - 使用変数：`player.payoff`（報酬額）
   - 分析内容：
     - 条件別の平均報酬額と標準偏差の計算
     - t検定による条件間の報酬差の統計的検定
   - データ処理：
     - 参加者フィルタリング（`participant.visited == 1`）
     - コントロール条件の16ラウンド目を除外
     - 報酬データの抽出と統合

4. **選好の自己評価と実際の行動の整合性分析** (`payoff_preference_analysis/payoff_preference_analysis.R`)

* 重要度: ★★★★★
* 分析手法: 多項ロジスティック回帰分析
* 使用変数:
	+ 説明変数: selfish_preference, equality_preference, efficiency_preference, competitive_preference
	+ 目的変数: player.choice（X/Y選択）
	+ 調整変数: condition（AI/Control）, gender, age
* 意義: 選好の自己申告が実際の選択行動とどの程度一致するかを検証。特にAI条件で申告選好と行動の乖離が大きい場合、AIの提示が認知バイアスを誘発している可能性を示唆。意思決定プロセスの解明に直結する核心分析。
* **分析ステップ**:
  1. 実験データとアンケートデータのマージ
  2. 選択行動の因子化（Y選択を基準カテゴリ）
  3. 自己評価変数の標準化（zスコア変換）
  4. 条件×選好の交互作用項の追加
  5. モデル選択（AIC/BICに基づく変数選択）
  6. 多重共線性のチェック（VIF計算）
  7. オッズ比と95%信頼区間の算出
  8. 限界効果プロットの作成

5. **利他性スコアの決定要因分析** (`altruism_analysis/altruism_analysis.R`) 

* 重要度: ★★★★☆
* 分析手法: 階層線形モデル（HLM）
* 使用変数:
	+ 従属変数: payoff_receiver（受け手への配分額）
	+ 独立変数: equality_preference, tech_trust, ai_satisfaction
	+ 個人効果: participant.id
* 意義: 受け手への配分額を利他性の指標とし、個人の平等選好やAIへの信頼度が実際の利他的行動に与える影響を定量化。実験介入効果のメカニズム解明に不可欠。
＊AI条件のみ
* **分析ステップ**:
  1. 個人内平均利他性スコアの算出
  2. 変数のセンタリング（グループ平均中心化）
  3. ランダム切片モデルの構築
  4. ランダム係数モデルへの拡張
  5. モデル比較（尤度比検定）
  6. 残差の正規性検定（QQプロット）
  7. 個人間変動の可視化（ランダム効果プロット）
  8. 効果量の計算（ICC）

6. **条件と所要時間の分析** (`time_analysis/time_analysis.R`)

* 重要度: ★★★★☆  
* 分析手法: 線形混合効果モデル / マン・ホイットニーU検定  
* 使用変数:  
  + 従属変数: decision_time_sec（意思決定所要時間）  
  + 独立変数: condition（AI/Control）  
  + 調整変数: round_number, gender, age  
  + ランダム効果: participant.id  

* **意義**:  
  - AI条件が意思決定速度に与える影響を定量化  
  - 時間圧力が利他性スコアに与える影響の媒介分析  
  - 実験操作の認知負荷検証（過度な時間延長は学習効果低下のリスク）  

* **分析ステップ**:  
  1. ページ遷移データの前処理  
  2. メインデータとのマージ
  3. 外れ値処理（中央値±3MAD）
  4. 混合効果モデル構築
  5. 残差診断（正規性・等分散性）  
  6. 効果量計算（Cohen's d）  
  7. 交互作用効果検証（条件×ラウンド数）  
  8. 結果可視化（箱ひげ図＋個別データポイント）  

* **解釈上の注意点**:  
  - 所要時間が短い≠意思決定の質が低い（速度と正確性のトレードオフを考慮）  
  - ラウンド進行に伴う学習効果を統計的に調整する必要  
  - マウス操作時間と認知処理時間を区別できない測定限界  

* **追加検証案**:  
  - 時間圧力が選択一貫性に与える影響（Gini係数による評価）  
  - 初期意思決定時間と最終選択の関連性分析  
  - 時間データと眼球運動データのマルチモーダル分析