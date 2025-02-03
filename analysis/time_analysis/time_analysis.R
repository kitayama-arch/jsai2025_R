# 必要なパッケージの読み込み
library(tidyverse)
library(lme4)
library(car)
library(ggplot2)
library(knitr)
library(rmarkdown)

# データの読み込み
# AI条件の独裁者ゲームデータ
data_0117_3 <- read_csv("../../AI2025_data/20250117_3/dictator_app_2025-01-17.csv")
data_0120_5 <- read_csv("../../AI2025_data/20250120_5/dictator_app_2025-01-20.csv")
# コントロール条件の独裁者ゲームデータ
data_0117_4 <- read_csv("../../AI2025_data/20250117_4/Base_dictator_app_2025-01-17.csv")
data_0120_4 <- read_csv("../../AI2025_data/20250120_4/Base_dictator_app_2025-01-20.csv")

# PageTimesデータの読み込み
page_times_0117_3 <- read_csv("../../AI2025_data/20250117_3/PageTimes-2025-01-17.csv")
page_times_0120_5 <- read_csv("../../AI2025_data/20250120_5/PageTimes-2025-01-20.csv")
page_times_0117_4 <- read_csv("../../AI2025_data/20250117_4/PageTimes-2025-01-17 (1).csv")
page_times_0120_4 <- read_csv("../../AI2025_data/20250120_4/PageTimes-2025-01-20.csv")

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

# 所要時間計算関数の改善
calculate_decision_time <- function(page_times, condition) {
  app_name <- if(condition == "AI") "dictator_app" else "Base_dictator_app"
  
  decisions <- page_times %>%
    filter(
      app_name == !!app_name,
      page_name %in% c("Decision", "Results")
    ) %>%
    arrange(participant_code, round_number, epoch_time_completed) %>%
    group_by(participant_code, round_number) %>%
    summarise(
      decision_time = diff(epoch_time_completed)[1],
      .groups = "drop"
    ) %>%
    mutate(condition = condition)
  
  return(decisions)
}

# 時間データの計算
time_data_0117_3 <- calculate_decision_time(page_times_0117_3, "AI")
time_data_0120_5 <- calculate_decision_time(page_times_0120_5, "AI")
time_data_0117_4 <- calculate_decision_time(page_times_0117_4, "Control")
time_data_0120_4 <- calculate_decision_time(page_times_0120_4, "Control")

# 時間データの統合
time_data <- bind_rows(
  time_data_0117_3,
  time_data_0120_5,
  time_data_0117_4,
  time_data_0120_4
)

# データの確認と前処理
print("PageTimes データの確認:")
print("AI条件 (0117_3):")
print(summarise(page_times_0117_3, n = n()))
print("\nAI条件 (0120_5):")
print(summarise(page_times_0120_5, n = n()))
print("\nControl条件 (0117_4):")
print(summarise(page_times_0117_4, n = n()))
print("\nControl条件 (0120_4):")
print(summarise(page_times_0120_4, n = n()))

# 各条件のデータ確認
print("\nデバッグ -  AI 条件の時間データ:")
print(summary(time_data_0117_3))
print("\nデバッグ -  AI 条件の時間データ:")
print(summary(time_data_0120_5))
print("\nデバッグ -  Control 条件の時間データ:")
print(summary(time_data_0117_4))
print("\nデバッグ -  Control 条件の時間データ:")
print(summary(time_data_0120_4))

print("\n統合後の時間データの確認:")
print(summary(time_data))

# メインデータの統合
merged_data <- bind_rows(
  data_0117_3_clean %>% mutate(condition = "AI"),
  data_0120_5_clean %>% mutate(condition = "AI"),
  data_0117_4_clean %>% mutate(condition = "Control"),
  data_0120_4_clean %>% mutate(condition = "Control")
) %>%
  left_join(time_data, 
            by = c("participant.code" = "participant_code", 
                  "subsession.round_number" = "round_number",
                  "condition"))

# 結合後のデータ確認
print("\n結合後のデータの確認:")
print(summary(merged_data$decision_time))
print(table(merged_data$condition, is.na(merged_data$decision_time)))

# 基本統計量の計算
summary_stats <- merged_data %>%
  group_by(condition) %>%
  summarise(
    mean_time = mean(decision_time, na.rm = TRUE),
    sd_time = sd(decision_time, na.rm = TRUE),
    median_time = median(decision_time, na.rm = TRUE),
    n_total = n(),
    n_valid = sum(!is.na(decision_time))
  )

print("\n基本統計量:")
print(summary_stats)

# データのクリーニング
model_data <- merged_data %>%
  filter(!is.na(decision_time)) %>%
  filter(decision_time > 0) %>%  # 0秒は除外
  filter(decision_time <= quantile(decision_time, 0.99, na.rm = TRUE)) %>%  # 上位1%を外れ値として除外
  mutate(
    condition = factor(condition, levels = c("Control", "AI")),
    participant.code = factor(participant.code)
  )

print("\nモデル用データの確認:")
print(summary(model_data))

# 混合効果モデルの実行
model <- lmer(decision_time ~ condition + (1|participant.code), data = model_data)
print("\n混合効果モデルの結果:")
print(summary(model))

# 可視化
p1 <- ggplot(model_data, aes(x = condition, y = decision_time, fill = condition)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Decision Time by Condition",
    y = "Decision Time (seconds)",
    x = "Condition"
  )
ggsave("decision_time_boxplot.png", plot = p1, width = 8, height = 6)

# 密度プロット
p2 <- ggplot(model_data, aes(x = decision_time, fill = condition)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Distribution of Decision Times by Condition",
    x = "Decision Time (seconds)",
    y = "Density"
  )
ggsave("decision_time_density.png", plot = p2, width = 8, height = 6)

# 結果をファイルに保存
sink("analysis_results.txt")
cat("時間分析の結果\n\n")
cat("1. 基本統計量:\n")
print(summary_stats)
cat("\n2. 混合効果モデルの結果:\n")
print(summary(model))
cat("\n3. データの要約:\n")
print(summary(model_data))
sink() 