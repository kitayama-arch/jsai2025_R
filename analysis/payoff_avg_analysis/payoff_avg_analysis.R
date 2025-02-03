# 配分額の分析
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

# player.payoff列を抽出して統合
payoff_ai <- bind_rows(
  data_0117_3_clean %>% select(player.payoff),
  data_0120_5_clean %>% select(player.payoff)
)

payoff_base <- bind_rows(
  data_0117_4_clean %>% select(player.payoff),
  data_0120_4_clean %>% select(player.payoff)
)

# 平均値と標準偏差を計算
ai_mean <- mean(payoff_ai$player.payoff)
ai_sd <- sd(payoff_ai$player.payoff)
base_mean <- mean(payoff_base$player.payoff)
base_sd <- sd(payoff_base$player.payoff)

# 結果の出力
cat("AI条件の平均配分額:", round(ai_mean, 2), "（SD =", round(ai_sd, 2), "）\n")
cat("コントロール条件の平均配分額:", round(base_mean, 2), "（SD =", round(base_sd, 2), "）\n")

# t検定の実行
t_test_result <- t.test(payoff_ai$player.payoff, payoff_base$player.payoff)

# t検定の結果を出力
cat("\nt検定の結果:\n")
print(t_test_result)

# 結果の可視化
# 箱ひげ図
p1 <- ggplot() +
  geom_boxplot(data = payoff_ai, aes(x = "AI", y = player.payoff)) +
  geom_boxplot(data = payoff_base, aes(x = "Control", y = player.payoff)) +
  theme_minimal() +
  labs(
    title = "配分額の分布",
    x = "条件",
    y = "配分額（円）"
  )
ggsave("payoff_boxplot.png", plot = p1, width = 8, height = 6)

# 密度プロット
p2 <- ggplot() +
  geom_density(data = payoff_ai, aes(x = player.payoff, fill = "AI"), alpha = 0.5) +
  geom_density(data = payoff_base, aes(x = player.payoff, fill = "Control"), alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "配分額の分布密度",
    x = "配分額（円）",
    y = "密度",
    fill = "条件"
  )
ggsave("payoff_density.png", plot = p2, width = 8, height = 6)

# 結果をファイルに保存
sink("payoff_analysis_results.txt")
cat("配分額分析の結果\n\n")
cat("1. 基本統計量:\n")
cat("AI条件の平均配分額:", round(ai_mean, 2), "（SD =", round(ai_sd, 2), "）\n")
cat("コントロール条件の平均配分額:", round(base_mean, 2), "（SD =", round(base_sd, 2), "）\n")
cat("\n2. t検定の結果:\n")
print(t_test_result)
sink() 