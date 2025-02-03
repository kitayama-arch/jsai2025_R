# AI2025実験分析

社会的選好と選択行動の分析結果

## リポジトリ構造

```
AI2025_R/
├── README.md                          # このファイル
├── .gitignore                         # Git除外設定
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

1. **条件間の比の検定** (`AI2025_Hinokentei.Rmd`)
   - 使用変数：`player.choice`（X/Y選択）
   - 分析内容：
     - AI条件とコントロール条件での選択比率（特にY選択）の比較
     - コンティンジェンシー表による比率の差の検定
     - 日付別（2025-01-17, 2025-01-20）のデータを統合して分析
   - データ処理：
     - 参加者フィルタリング（`participant.visited == 1`）
     - コントロール条件の16ラウンド目を除外
     - 選択データの抽出と統合（`bind_rows`による日付別データの結合）

2. **社会的選好分析** (`AI2025_socialPreference_analysis.R`)
   - 使用変数：
     - `player.payoff_dictator`, `player.payoff_receiver`（報酬額）
     - 不平等指標：`s`（受け手有利）, `r`（独裁者有利）
   - 分析内容：
     - 効用関数の最尤推定による選好パラメータ（α, β）の推定
     - 条件間での社会的選好パターンの比較
     - 個人レベルでの選好パラメータの推定と分類
   - データ処理：
     - 選択を数値化（`choice_numeric`）
     - 不平等指標の計算
     - 効用関数の定義と最適化

3. **条件間の平均の差の分析** (`AI2025_payoffAvg.Rmd`)
   - 使用変数：`player.payoff`（報酬額）
   - 分析内容：
     - 条件別の平均報酬額と標準偏差の計算
     - t検定による条件間の報酬差の統計的検定
   - データ処理：
     - 参加者フィルタリング（`participant.visited == 1`）
     - コントロール条件の16ラウンド目を除外
     - 報酬データの抽出と統合

### 優先度1：選好の自己評価と実際の行動の整合性分析

* 重要度: ★★★★★
* 分析手法: 多項ロジスティック回帰分析
* 使用変数:
	+ 説明変数: selfish_preference, equality_preference, efficiency_preference, competitive_preference
	+ 目的変数: player.choice（X/Y選択）
	+ 調整変数: condition（AI/Control）, gender, age
* 意義: 選好の自己申告が実際の選択行動とどの程度一致するかを検証。特にAI条件で申告選好と行動の乖離が大きい場合、AIの提示が認知バイアスを誘発している可能性を示唆。意思決定プロセスの解明に直結する核心分析。

### 優先度2：利他性スコアの決定要因分析

* 重要度: ★★★★☆
* 分析手法: 階層線形モデル（HLM）
* 使用変数:
	+ 従属変数: payoff_receiver（受け手への配分額）
	+ 独立変数: equality_preference, tech_trust, ai_satisfaction
	+ 個人効果: participant.id
* 意義: 受け手への配分額を利他性の指標とし、個人の平等選好やAIへの信頼度が実際の利他的行動に与える影響を定量化。実験介入効果のメカニズム解明に不可欠。

### 優先度3：AI理解度と予測精度評価の関連性分析

* 重要度: ★★★☆☆
* 分析手法: 順序ロジットモデル
* 使用変数:
	+ 目的変数: prediction_accuracy（7段階評価）
	+ 説明変数: rf_understanding, ai_understanding, gakunen
* 意義: AIアルゴリズムの理解度が予測精度の主観的評価に与える影響を解明。教育レベル（学年）の調整効果を検出可能。

### 優先度4：時間的変化に伴う選択パターンの分析

* 重要度: ★★☆☆☆
* 分析手法: 時系列クラスタリング
* 使用変数:
	+ subsession.round_number
	+ player.choice
	+ scenario（選択肢の特性）
* 意義: ラウンド進行に伴う選択戦略の変化を可視化。学習効果や疲労効果の検出により、実験デザインの改善に寄与。

### 優先度5：属性別報酬分配パターンの比較

* 重要度: ★★☆☆☆
* 分析手法: ANOVA + 事後検定
* 使用変数:
	+ final_total_payoff
	+ gender, gakubu, gakunen
* 意義: 性別・学部・学年による報酬獲得パターンの差異を検出。サンプルの代表性評価に有用。
