# 選択行動の分析
# Author: Taito Hirano
# Date: 2025-01-24

# 必要なライブラリの読み込み
library(tidyverse)

# 作業ディレクトリのパスを設定
output_dir <- file.path("analysis", "choice_analysis")

# データの読み込み
# AI条件のデータ
data_0117_3 <- read_csv("../../AI2025_data/20250117_3/dictator_app_2025-01-17.csv")
data_0120_5 <- read_csv("../../AI2025_data/20250120_5/dictator_app_2025-01-20.csv")
# コントロール条件のデータ
data_0117_4 <- read_csv("../../AI2025_data/20250117_4/Base_dictator_app_2025-01-17.csv")
data_0120_4 <- read_csv("../../AI2025_data/20250120_4/Base_dictator_app_2025-01-20.csv")

# データクリーニング
# 実際に参加した参加者のデータのみを抽出
data_0117_3_clean <- data_0117_3 %>%
  filter(participant.visited == 1)

data_0120_5_clean <- data_0120_5 %>%
  filter(participant.visited == 1)

# ベースライン条件ではラウンド16を除外
data_0117_4_clean <- data_0117_4 %>%
  filter(participant.visited == 1) %>%
  filter(subsession.round_number != 16)

data_0120_4_clean <- data_0120_4 %>%
  filter(participant.visited == 1) %>%
  filter(subsession.round_number != 16)

# player.choice列を抽出
choices_0117_3 <- data_0117_3_clean %>%
  select(player.choice, subsession.round_number)

choices_0120_5 <- data_0120_5_clean %>%
  select(player.choice, subsession.round_number)

choices_0117_4 <- data_0117_4_clean %>%
  select(player.choice, subsession.round_number)

choices_0120_4 <- data_0120_4_clean %>%
  select(player.choice, subsession.round_number)

# データの統合
combined_choices_ai <- bind_rows(
  mutate(choices_0117_3, source = "2025-01-17"),
  mutate(choices_0120_5, source = "2025-01-20")
) %>%
  mutate(condition = "AI")

combined_choices_base <- bind_rows(
  mutate(choices_0117_4, source = "2025-01-17"),
  mutate(choices_0120_4, source = "2025-01-20")
) %>%
  mutate(condition = "Control")

# 全データの統合
all_choices <- bind_rows(combined_choices_ai, combined_choices_base)

# AI条件の集計
ai_total <- nrow(combined_choices_ai)
ai_y_count <- sum(combined_choices_ai$player.choice == "Y")
ai_proportion <- ai_y_count / ai_total

# コントロール条件の集計
base_total <- nrow(combined_choices_base)
base_y_count <- sum(combined_choices_base$player.choice == "Y")
base_proportion <- base_y_count / base_total

# 結果の出力
cat("選択行動の分析結果\n\n")
cat("1. AI条件\n")
cat("  全体の選択数:", ai_total, "\n")
cat("  Yの選択数:", ai_y_count, "\n")
cat("  Y選択率:", round(ai_proportion * 100, 2), "%\n\n")

cat("2. コントロール条件\n")
cat("  全体の選択数:", base_total, "\n")
cat("  Yの選択数:", base_y_count, "\n")
cat("  Y選択率:", round(base_proportion * 100, 2), "%\n\n")

# カイ二乗検定
contingency_table <- matrix(c(
  ai_y_count, ai_total - ai_y_count,
  base_y_count, base_total - base_y_count
), nrow = 2)

chi_square_test <- chisq.test(contingency_table)

cat("3. カイ二乗検定の結果\n")
print(chi_square_test)

# ラウンドごとの選択率の分析
round_choices <- all_choices %>%
  group_by(condition, subsession.round_number) %>%
  summarise(
    total = n(),
    y_count = sum(player.choice == "Y"),
    y_proportion = y_count / total,
    .groups = "drop"
  )

# ファイルの存在確認
payoff_scenarios_file <- "Experiment/payoff_scenarios_analysis.csv"
if (!file.exists(payoff_scenarios_file)) {
  stop(paste("ファイルが見つかりません:", payoff_scenarios_file))
}

# データ読み込みのエラーハンドリング
tryCatch({
  payoff_scenarios <- read_csv(payoff_scenarios_file)
}, error = function(e) {
  stop(paste("ファイルの読み込みに失敗しました:", e$message))
})

# 参加者レベルの分析を追加
participant_level_analysis <- function(data, scenarios) {
  # シナリオとデータの結合
  data_with_scenarios <- data %>%
    select(participant.code, condition, subsession.round_number, player.choice) %>%
    mutate(
      is_training = TRUE,  # 実験データは全て訓練データ
      Game = subsession.round_number  # カラム名の対応付け
    ) %>%
    left_join(
      scenarios %>% 
        select(Game, Is_Training, Category_1_Slope),  # Is_Restrictedの代わりにCategory_1_Slopeを使用
      by = c("Game" = "Game",
             "is_training" = "Is_Training")
    ) %>%
    mutate(
      Is_Restricted = Category_1_Slope == "Selfish"  # Selfishの場合をRestricted Sampleとする
    )
  
  # 参加者ごとの選択頻度の計算
  participant_choices <- data_with_scenarios %>%
    group_by(participant.code, condition) %>%
    summarise(
      # 全サンプルでの選択頻度
      total_choices = n(),
      x_choices = sum(player.choice == "X"),
      x_proportion = x_choices / total_choices,
      
      # Restricted sampleでの選択頻度
      restricted_total = sum(Is_Restricted == TRUE, na.rm = TRUE),
      restricted_x = sum(player.choice == "X" & Is_Restricted == TRUE, na.rm = TRUE),
      restricted_x_proportion = if_else(restricted_total > 0, 
                                      restricted_x / restricted_total, 
                                      NA_real_),
      .groups = "drop"
    )
  
  return(participant_choices)
}

# 参加者レベルの分析実行
all_data <- bind_rows(
  data_0117_3_clean %>% mutate(condition = "AI"),
  data_0120_5_clean %>% mutate(condition = "AI"),
  data_0117_4_clean %>% mutate(condition = "Control"),
  data_0120_4_clean %>% mutate(condition = "Control")
)

participant_results <- participant_level_analysis(all_data, payoff_scenarios)

# 制限ありサンプルでの選択データの抽出
restricted_choices <- all_data %>%
  mutate(
    is_training = TRUE,
    Game = subsession.round_number
  ) %>%
  left_join(
    payoff_scenarios %>% 
      select(Game, Is_Training, Category_1_Slope),
    by = c("Game" = "Game",
           "is_training" = "Is_Training")
  ) %>%
  filter(Category_1_Slope == "Selfish")

# 制限ありサンプルでの集計
restricted_ai_total <- sum(restricted_choices$condition == "AI")
restricted_ai_y_count <- sum(restricted_choices$player.choice == "Y" & restricted_choices$condition == "AI")
restricted_ai_proportion <- restricted_ai_y_count / restricted_ai_total

restricted_base_total <- sum(restricted_choices$condition == "Control")
restricted_base_y_count <- sum(restricted_choices$player.choice == "Y" & restricted_choices$condition == "Control")
restricted_base_proportion <- restricted_base_y_count / restricted_base_total

# 制限ありサンプルでのカイ二乗検定
restricted_contingency_table <- matrix(c(
  restricted_ai_y_count, restricted_ai_total - restricted_ai_y_count,
  restricted_base_y_count, restricted_base_total - restricted_base_y_count
), nrow = 2)

restricted_chi_square_test <- chisq.test(restricted_contingency_table)

# t検定の実行
# 全サンプル
t_test_all <- t.test(
  x_proportion ~ condition,
  data = participant_results
)

# Restricted sample
t_test_restricted <- t.test(
  restricted_x_proportion ~ condition,
  data = participant_results
)

# 統計量の計算
ai_stats_all <- participant_results %>% 
  filter(condition == "AI") %>%
  summarise(
    mean = mean(x_proportion),
    se = sd(x_proportion) / sqrt(n()),
    n = n()
  )

control_stats_all <- participant_results %>% 
  filter(condition == "Control") %>%
  summarise(
    mean = mean(x_proportion),
    se = sd(x_proportion) / sqrt(n()),
    n = n()
  )

ai_stats_restricted <- participant_results %>% 
  filter(condition == "AI") %>%
  summarise(
    mean = mean(restricted_x_proportion, na.rm = TRUE),
    se = sd(restricted_x_proportion, na.rm = TRUE) / sqrt(sum(!is.na(restricted_x_proportion))),
    n = sum(!is.na(restricted_x_proportion))
  )

control_stats_restricted <- participant_results %>% 
  filter(condition == "Control") %>%
  summarise(
    mean = mean(restricted_x_proportion, na.rm = TRUE),
    se = sd(restricted_x_proportion, na.rm = TRUE) / sqrt(sum(!is.na(restricted_x_proportion))),
    n = sum(!is.na(restricted_x_proportion))
  )

# 結果をMarkdownファイルに保存
output_file <- file.path(output_dir, "choice_analysis_results.md")

# 既存のファイルがある場合は削除
if (file.exists(output_file)) {
  file.remove(output_file)
}

# Markdownファイルへの出力開始
sink(output_file)

# 基本統計量
cat("# 選択行動の分析結果\n\n")
cat("## 1. 基本統計量\n\n")
cat("### AI条件\n")
cat("- 全体の選択数:", ai_total, "\n")
cat("- Yの選択数:", ai_y_count, "\n")
cat("- Y選択率:", round(ai_proportion * 100, 2), "%\n\n")

cat("### コントロール条件\n")
cat("- 全体の選択数:", base_total, "\n")
cat("- Yの選択数:", base_y_count, "\n")
cat("- Y選択率:", round(base_proportion * 100, 2), "%\n\n")

# 統計的検定
cat("## 2. 統計的検定\n\n")
cat("### カイ二乗検定の結果\n")
cat("#### 全サンプル\n")
cat("- χ²値:", round(chi_square_test$statistic, 3), "\n")
cat("- 自由度:", chi_square_test$parameter, "\n")
cat("- p値:", format.pval(chi_square_test$p.value, digits = 3), "\n\n")

cat("#### 制限ありサンプル\n")
cat("- χ²値:", round(restricted_chi_square_test$statistic, 3), "\n")
cat("- 自由度:", restricted_chi_square_test$parameter, "\n")
cat("- p値:", format.pval(restricted_chi_square_test$p.value, digits = 3), "\n\n")

cat("### t検定の結果\n\n")
cat("#### 全サンプル\n")
cat("- t値:", round(t_test_all$statistic, 3), "\n")
cat("- 自由度:", round(t_test_all$parameter, 3), "\n")
cat("- p値:", format.pval(t_test_all$p.value, digits = 3), "\n")
cat("- 95%信頼区間: [", 
    round(t_test_all$conf.int[1], 3), ", ",
    round(t_test_all$conf.int[2], 3), "]\n\n")

cat("#### 制限ありサンプル\n")
cat("- t値:", round(t_test_restricted$statistic, 3), "\n")
cat("- 自由度:", round(t_test_restricted$parameter, 3), "\n")
cat("- p値:", format.pval(t_test_restricted$p.value, digits = 3), "\n")
cat("- 95%信頼区間: [", 
    round(t_test_restricted$conf.int[1], 3), ", ",
    round(t_test_restricted$conf.int[2], 3), "]\n\n")

# 参加者レベルの分析
cat("## 3. 参加者レベルの分析\n\n")
cat("### AI条件\n")
cat("#### 全サンプル\n")
cat("- 平均X選択率:", round(ai_stats_all$mean * 100, 2), "%\n")
cat("- 標準誤差:", round(ai_stats_all$se * 100, 2), "%\n")
cat("- サンプルサイズ:", ai_stats_all$n, "\n\n")

cat("#### Restricted Sample\n")
cat("- 平均X選択率:", round(ai_stats_restricted$mean * 100, 2), "%\n")
cat("- 標準誤差:", round(ai_stats_restricted$se * 100, 2), "%\n")
cat("- サンプルサイズ:", ai_stats_restricted$n, "\n\n")

cat("### コントロール条件\n")
cat("#### 全サンプル\n")
cat("- 平均X選択率:", round(control_stats_all$mean * 100, 2), "%\n")
cat("- 標準誤差:", round(control_stats_all$se * 100, 2), "%\n")
cat("- サンプルサイズ:", control_stats_all$n, "\n\n")

cat("#### Restricted Sample\n")
cat("- 平均X選択率:", round(control_stats_restricted$mean * 100, 2), "%\n")
cat("- 標準誤差:", round(control_stats_restricted$se * 100, 2), "%\n")
cat("- サンプルサイズ:", control_stats_restricted$n, "\n\n")

# 図表
cat("## 4. 図表\n\n")
cat("### ラウンドごとの選択率\n")
cat("![ラウンドごとの選択率](round_choice_proportion.png)\n\n")

cat("### 参加者レベルの選択率分布\n")
cat("![参加者レベルの選択率分布](participant_level_choice_proportion.png)\n\n")

cat("### 条件ごとの選択率箱ひげ図\n")
cat("![条件ごとの選択率箱ひげ図](choice_proportion_boxplot.png)\n\n")

# sink()を閉じる
sink()

# 結果の可視化（sink()の後に実行）
# ラウンドごとの選択率の推移
p1 <- ggplot(round_choices, aes(x = subsession.round_number, y = y_proportion, color = condition)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(
    title = "ラウンドごとのY選択率",
    x = "ラウンド",
    y = "Y選択率",
    color = "条件"
  )
ggsave(file.path(output_dir, "round_choice_proportion.png"), plot = p1, width = 10, height = 6)

# 条件ごとの選択率の箱ひげ図
p2 <- ggplot(round_choices, aes(x = condition, y = y_proportion, fill = condition)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "条件ごとのY選択率分布",
    x = "条件",
    y = "Y選択率",
    fill = "条件"
  )
ggsave(file.path(output_dir, "choice_proportion_boxplot.png"), plot = p2, width = 8, height = 6)

# 参加者レベルの選択率の分布
p3 <- ggplot(participant_results, aes(x = condition)) +
  geom_boxplot(aes(y = x_proportion, fill = "全サンプル")) +
  geom_boxplot(aes(y = restricted_x_proportion, fill = "Restricted sample")) +
  theme_minimal() +
  labs(
    title = "参加者レベルの選択率分布",
    x = "条件",
    y = "Option X選択率",
    fill = "サンプル"
  )
ggsave(file.path(output_dir, "participant_level_choice_proportion.png"), plot = p3, width = 8, height = 6)
