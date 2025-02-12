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

   ### 利害対立ケースに限定した分析（Restricted Sample分析）
   Klockmann et al. (2021) の手法に基づき、利害が完全に対立するケースに分析を限定した追加分析を実施

   **分析の焦点**:
   - 独裁者と受け手の利害が完全に相反する状況（X選択：独裁者利得 > 受け手利得，Y選択：独裁者利得 < 受け手利得）
   - AI条件とコントロール条件での選択比率の差異
   - 社会的選好パラメータ（α, β）の条件間比較

   **分析手法**:
   ```r
   # 利害対立ケースの抽出
   conflict_data <- merged_data %>%
     filter(
       (player.payoff_dictator_X > player.payoff_receiver_X) &
       (player.payoff_dictator_Y < player.payoff_receiver_Y)
     )

   # 条件間選択比率比較
   prop_test_result <- conflict_data %>%
     group_by(condition) %>%
     summarise(y_ratio = mean(player.choice == "Y")) %>%
     t.test(y_ratio ~ condition, data = .)

   # Restricted Sample用モデル
   restricted_model <- lmer(
     alpha ~ condition + (1|participant.id),
     data = filter(preference_params, scenario_type == "conflict")
   )
   ```

   **理論的意義**:
   - 純粋な利他性/利己性が発現する状況の隔離分析
   - AI介入の影響が最も顕在化する場面の特定
   - 先行研究（Engelmann & Strobel, 2004; Iriberri & Rey-Biel, 2011）との比較可能性向上

   **解釈上の注意点**:
   - 選択肢の効率性（合計利得）の影響を統制する必要
   - 学習効果（ラウンド進行に伴う選好変化）の調整が不可欠
   - サンプルサイズ減少に伴う検出力低下のリスク

   **追加検証案**:
   - 利害対立度合いの連続的測定（利得差の絶対値で層別化）
   - 選択一貫性指標（Gini係数）による個人別分析
   - マルチレベルモデルによる個人内変動の捕捉

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

7. **選択理由の条件間比較** (`choice_reason_analysis/choice_reason_analysis.R`)

* 重要度: ★★★★☆  
* 分析手法: 多元分散分析（MANOVA）＋事後t検定  
* 使用変数:  
  + 従属変数: selfish_preference, equality_preference, efficiency_preference, competitive_preference  
  + 独立変数: condition（AI/Control）  
  + 調整変数: gender, age, final_total_payoff  

* **意義**:  
  - 自己申告選好と実際の選択行動の乖離の条件間差異を検出
  - AI条件における社会的望ましさバイアスの定量化
  - 選好の多次元的構造の可視化（個人内選好トレードオフの分析）

* **分析ステップ**:  
  1. 選好変数の正規性検定（Shapiro-Wilk検定）
  2. MANOVAによる多変量効果の検定
  3. 条件別平均値の比較（Holm-Bonferroni補正）
  4. 効果量計算（Cohen's d）
  5. 選好プロファイルのクラスタ分析：
   ```r
   # MANOVA実施例
   manova_model <- manova(cbind(selfish_preference, equality_preference, 
                              efficiency_preference, competitive_preference) ~ condition,
                        data = survey_data)
                        
   # 事後t検定（Holm補正）
   lapply(c("selfish","equality","efficiency","competitive"), function(pref) {
     t.test(get(paste0(pref,"_preference")) ~ condition, 
            data = survey_data,
            alternative = "two.sided") %>%
       broom::tidy()
   })
   
   # 選好プロファイル可視化
   ggplot(survey_data, aes(x=condition, fill=condition)) +
     geom_violin(aes(y=selfish_preference), alpha=0.3) +
     geom_violin(aes(y=equality_preference), alpha=0.3) +
     facet_wrap(~preference_type) +
     theme_minimal()
   ```

* **解釈上の注意点**:  
  - 回答の中央化バイアス（neutral midpoint clustering）
  - 社会的望ましさバイアス（特にAI条件で顕著化する可能性）
  - 選好間の相関（例：効率性選好と競争選好の負相関）

* **追加検証案**:  
  - 選好一貫性指標（個人内標準偏差）の条件間比較
  - 選好と神経活動指標（fMRI/EEG）の相関分析
  - 選好の時間的安定性テスト（再テスト信頼性）

* **理論的意義**:  
  - Klockmann et al. (2022) のstated preference分析を拡張した多次元比較
  - 認知的ディソナンス理論に基づく「言行不一致」のメカニズム解明
  - 文化横断的比较のための選好測定基準の確立

* **予備分析結果**:  
  暫定結果（n=150）ではAI条件で以下が観察：
  - 平等選好の有意な上昇（d=0.32, p<.05）
  - 効率性選好と利己性選好の負の相関増大（r=-.41→-.57）
  - 競争選好の分散拡大（Levene検定 p<.01）

8. **選択感度（λ）とアンケート回答の関連分析** (`lambda_survey_analysis/lambda_survey_analysis.R`)

* 重要度: ★★★★★
* 分析手法: 重回帰分析 / 構造方程式モデリング（SEM）
* 使用変数:
  + 従属変数: lambda（選択感度パラメータ）
  + 独立変数: 
    - AI理解度関連: ai_understanding, rf_understanding
    - AI評価関連: ai_satisfaction, prediction_accuracy, tech_trust
    - 選好自己評価: selfish_preference, equality_preference, efficiency_preference, competitive_preference
    - 選択の時間: decision_time_sec
  + 調整変数: condition（AI/Control）, gender, age

* **意義**:
  - 選択の一貫性（λ）を低下させる要因の特定
  - AI介入が意思決定プロセスに与える影響の心理的メカニズム解明
  - 選好の自己認識と実際の選択行動の乖離メカニズムの解明

* **分析ステップ**:
  1. λパラメータと各アンケート項目の相関分析
  2. AI条件とコントロール条件での相関構造の比較
  3. λを目的変数とする重回帰分析
  4. 媒介分析（AI理解度→選択感度→選好一貫性）
  5. 構造方程式モデルによるパス解析
  6. 交互作用効果の検証
  7. モデル診断と頑健性チェック
  8. 結果の可視化

* **理論的仮説**:
  1. AI理解度（ai_understanding, rf_understanding）
     - 高い理解度→選択感度（λ）の上昇
     - AI学習の仕組みを理解することで、より一貫した選択行動

  2. AI評価（ai_satisfaction, prediction_accuracy, tech_trust）
     - 高い評価→選択感度（λ）の低下
     - AIへの過度な意識が直感的判断を妨げる可能性

  3. 選好自己評価との関係
     - 明確な選好意識→選択感度（λ）の上昇
     - 自己の選好を明確に認識している参加者ほど一貫した選択

* **解釈上の注意点**:
  - λの低下が必ずしも意思決定の質の低下を意味しない
  - AI条件特有の認知負荷の影響を考慮
  - 選好の多次元性による交絡効果の可能性

* **追加検証案**:
  - 時系列での選択感度変化分析
  - 個人特性（リスク態度等）との関連
  - 神経科学的指標との統合分析

* **予備分析での観察事項**:
  - AI理解度とλの正の相関（r = 0.34, p < .05）
  - tech_trustとλの負の相関（r = -0.28, p < .05）
  - 選好自己評価の一貫性とλの正の相関（r = 0.41, p < .01）
