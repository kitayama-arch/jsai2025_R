# 選択行動の分析
# Author: Taito Hirano
# Date: 2025-01-24

# 必要なライブラリの読み込み
library(tidyverse)

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

# 結果の可視化
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
ggsave("round_choice_proportion.png", plot = p1, width = 10, height = 6)

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
ggsave("choice_proportion_boxplot.png", plot = p2, width = 8, height = 6)

# 結果をファイルに保存
sink("choice_analysis_results.txt")
cat("選択行動の分析結果\n\n")
cat("1. 基本統計量\n")
cat("AI条件:\n")
cat("  全体の選択数:", ai_total, "\n")
cat("  Yの選択数:", ai_y_count, "\n")
cat("  Y選択率:", round(ai_proportion * 100, 2), "%\n\n")
cat("コントロール条件:\n")
cat("  全体の選択数:", base_total, "\n")
cat("  Yの選択数:", base_y_count, "\n")
cat("  Y選択率:", round(base_proportion * 100, 2), "%\n\n")
cat("2. カイ二乗検定の結果\n")
print(chi_square_test)
cat("\n3. ラウンドごとの選択率\n")
print(round_choices)
sink() 