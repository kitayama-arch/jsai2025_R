# 配分額の分析
# Author: Taito Hirano
# Date: 2025-01-24

# 必要なライブラリの読み込み
library(tidyverse)

# データの読み込み
# AI条件のデータ
data_0117_3 <- read_csv("AI2025_data/20250117_3/dictator_app_2025-01-17.csv")
data_0120_5 <- read_csv("AI2025_data/20250120_5/dictator_app_2025-01-20.csv")
# コントロール条件のデータ
data_0117_4 <- read_csv("AI2025_data/20250117_4/Base_dictator_app_2025-01-17.csv")
data_0120_4 <- read_csv("AI2025_data/20250120_4/Base_dictator_app_2025-01-20.csv")

# payoff_scenariosの読み込み（restricted sample判定用）
payoff_scenarios <- read_csv("Experiment/payoff_scenarios_analysis.csv")

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

# データの統合と条件の追加
all_data <- bind_rows(
  data_0117_3_clean %>% mutate(condition = "AI"),
  data_0120_5_clean %>% mutate(condition = "AI"),
  data_0117_4_clean %>% mutate(condition = "Control"),
  data_0120_4_clean %>% mutate(condition = "Control")
)

# Restricted sampleの情報を追加
data_with_scenarios <- all_data %>%
  select(participant.code, condition, subsession.round_number, player.payoff) %>%
  mutate(is_training = TRUE) %>%  # 実験データは全て訓練データ
  left_join(
    payoff_scenarios %>% 
      select(Game, Is_Restricted, Is_Training),
    by = c("subsession.round_number" = "Game",
           "is_training" = "Is_Training")
  )

# 全サンプルの分析
payoff_summary_all <- data_with_scenarios %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean = mean(player.payoff),
    sd = sd(player.payoff),
    se = sd / sqrt(n)
  )

# Restricted sampleの分析
payoff_summary_restricted <- data_with_scenarios %>%
  filter(Is_Restricted == TRUE) %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean = mean(player.payoff),
    sd = sd(player.payoff),
    se = sd / sqrt(n)
  )

# t検定の実行
# 全サンプル
t_test_all <- t.test(player.payoff ~ condition, data = data_with_scenarios)

# Restricted sample
t_test_restricted <- t.test(
  player.payoff ~ condition, 
  data = data_with_scenarios %>% filter(Is_Restricted == TRUE)
)

# 結果の出力
cat("配分額の分析結果\n\n")
cat("1. 全サンプルでの分析\n")
cat("AI条件:\n")
with(payoff_summary_all %>% filter(condition == "AI"),
     cat(sprintf("  平均配分額: %.2f円（SE = %.2f, n = %d）\n", mean, se, n)))
cat("コントロール条件:\n")
with(payoff_summary_all %>% filter(condition == "Control"),
     cat(sprintf("  平均配分額: %.2f円（SE = %.2f, n = %d）\n", mean, se, n)))

cat("\nt検定結果（全サンプル）:\n")
print(t_test_all)

cat("\n2. Restricted sampleでの分析\n")
cat("AI条件:\n")
with(payoff_summary_restricted %>% filter(condition == "AI"),
     cat(sprintf("  平均配分額: %.2f円（SE = %.2f, n = %d）\n", mean, se, n)))
cat("コントロール条件:\n")
with(payoff_summary_restricted %>% filter(condition == "Control"),
     cat(sprintf("  平均配分額: %.2f円（SE = %.2f, n = %d）\n", mean, se, n)))

cat("\nt検定結果（Restricted sample）:\n")
print(t_test_restricted)

# 結果の可視化
# 全サンプルとRestricted sampleの箱ひげ図
p1 <- ggplot(data_with_scenarios, aes(x = condition, y = player.payoff)) +
  geom_boxplot(aes(fill = "全サンプル")) +
  geom_boxplot(data = data_with_scenarios %>% filter(Is_Restricted == TRUE),
               aes(fill = "Restricted sample")) +
  theme_minimal() +
  labs(
    title = "配分額の分布",
    x = "条件",
    y = "配分額（円）",
    fill = "サンプル"
  )
ggsave("analysis/payoff_avg_analysis/payoff_boxplot.png", plot = p1, width = 8, height = 6)

# 全サンプルとRestricted sampleの密度プロット
p2 <- ggplot() +
  # 全サンプル
  geom_density(data = data_with_scenarios %>% filter(condition == "AI"),
               aes(x = player.payoff, color = "AI (全サンプル)"), alpha = 0.3) +
  geom_density(data = data_with_scenarios %>% filter(condition == "Control"),
               aes(x = player.payoff, color = "Control (全サンプル)"), alpha = 0.3) +
  # Restricted sample
  geom_density(data = data_with_scenarios %>% filter(condition == "AI", Is_Restricted == TRUE),
               aes(x = player.payoff, color = "AI (Restricted)"), alpha = 0.3) +
  geom_density(data = data_with_scenarios %>% filter(condition == "Control", Is_Restricted == TRUE),
               aes(x = player.payoff, color = "Control (Restricted)"), alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "配分額の分布密度",
    x = "配分額（円）",
    y = "密度",
    color = "条件とサンプル"
  )
ggsave("analysis/payoff_avg_analysis/payoff_density.png", plot = p2, width = 8, height = 6)

# 結果をファイルに保存
sink("analysis/payoff_avg_analysis/payoff_analysis_results.txt")
cat("配分額分析の結果\n\n")
cat("1. 全サンプルでの分析\n")
cat("AI条件:\n")
with(payoff_summary_all %>% filter(condition == "AI"),
     cat(sprintf("  平均配分額: %.2f円（SE = %.2f, n = %d）\n", mean, se, n)))
cat("コントロール条件:\n")
with(payoff_summary_all %>% filter(condition == "Control"),
     cat(sprintf("  平均配分額: %.2f円（SE = %.2f, n = %d）\n", mean, se, n)))

cat("\nt検定結果（全サンプル）:\n")
print(t_test_all)

cat("\n2. Restricted sampleでの分析\n")
cat("AI条件:\n")
with(payoff_summary_restricted %>% filter(condition == "AI"),
     cat(sprintf("  平均配分額: %.2f円（SE = %.2f, n = %d）\n", mean, se, n)))
cat("コントロール条件:\n")
with(payoff_summary_restricted %>% filter(condition == "Control"),
     cat(sprintf("  平均配分額: %.2f円（SE = %.2f, n = %d）\n", mean, se, n)))

cat("\nt検定結果（Restricted sample）:\n")
print(t_test_restricted)
sink() 