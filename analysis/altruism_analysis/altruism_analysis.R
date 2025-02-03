# 必要なパッケージの読み込み
library(tidyverse)
library(lme4)
library(car)
library(ggplot2)
library(knitr)
library(rmarkdown)
library(gridExtra)
library(sjPlot)  # ランダム効果プロット用
library(performance)  # モデル診断用

# デバッグ用の関数
print_data_structure <- function(data, name) {
  cat("\n=== ", name, " ===\n")
  cat("Dimensions:", dim(data), "\n")
  cat("Column names:", paste(names(data), collapse = ", "), "\n")
  cat("Structure:\n")
  str(data)
  cat("\nSummary:\n")
  print(summary(data))
  cat("===========================\n")
}

# AI条件のデータのみ読み込み
data_0117_3_dict <- read_csv("../../AI2025_data/20250117_3/dictator_app_2025-01-17.csv")
data_0117_3_quest <- read_csv("../../AI2025_data/20250117_3/questionnaire_app_2025-01-17 (1).csv")
data_0120_5_dict <- read_csv("../../AI2025_data/20250120_5/dictator_app_2025-01-20.csv")
data_0120_5_quest <- read_csv("../../AI2025_data/20250120_5/questionnaire_app_2025-01-20.csv")

# データのクリーニングと前処理
clean_and_merge_data <- function(dict_data, quest_data, date) {
  # 実際に参加した参加者のデータのみを抽出
  dict_clean <- dict_data %>%
    filter(participant.visited == 1)
  
  quest_clean <- quest_data %>%
    filter(participant.visited == 1)
  
  # アンケートデータの抽出（AI関連の変数を追加）
  preferences <- quest_clean %>%
    select(participant.code,
           player.equality_preference,
           player.tech_trust,
           player.ai_satisfaction)
  
  # 欠損値の確認
  cat("\nMissing values in preferences for", date, ":\n")
  print(colSums(is.na(preferences)))
  
  # 利他性データの処理
  altruism <- dict_clean %>%
    select(participant.code,
           subsession.round_number,
           player.payoff_receiver) %>%  # 受け手への配分額
    mutate(
      date = date,
      player.payoff_receiver = as.numeric(player.payoff_receiver)
    )
  
  # 欠損値の確認
  cat("\nMissing values in altruism for", date, ":\n")
  print(colSums(is.na(altruism)))
  
  # データのマージ
  merged_data <- altruism %>%
    left_join(preferences, by = "participant.code") %>%
    # NAを含む行を除外
    drop_na()
  
  # マージ後のデータ確認
  cat("\nDimensions of merged data for", date, ":", dim(merged_data), "\n")
  
  return(merged_data)
}

# データの処理
ai_data_0117 <- clean_and_merge_data(data_0117_3_dict, data_0117_3_quest, "2025-01-17")
ai_data_0120 <- clean_and_merge_data(data_0120_5_dict, data_0120_5_quest, "2025-01-20")

# データ構造の確認
print_data_structure(ai_data_0117, "AI Data 2025-01-17")
print_data_structure(ai_data_0120, "AI Data 2025-01-20")

# 全データの結合
all_data <- bind_rows(
  ai_data_0117,
  ai_data_0120
) %>%
  mutate(
    participant.code = factor(participant.code),  # 参加者IDを因子型に変換
    date = factor(date)  # 実験日を因子型に変換
  )

# 変数の標準化
all_data <- all_data %>%
  mutate(
    across(
      c(player.equality_preference, player.tech_trust, player.ai_satisfaction),
      ~scale(as.numeric(.)) %>% as.vector
    )
  )

# 結合後のデータ構造の確認
print_data_structure(all_data, "Combined Data")

# データの要約統計量を確認
summary_stats <- all_data %>%
  group_by(date) %>%
  summarise(
    n_participants = n_distinct(participant.code),
    mean_payoff = mean(player.payoff_receiver),
    sd_payoff = sd(player.payoff_receiver),
    min_payoff = min(player.payoff_receiver),
    max_payoff = max(player.payoff_receiver),
    mean_tech_trust = mean(player.tech_trust),
    mean_ai_satisfaction = mean(player.ai_satisfaction)
  )

print("Summary Statistics:")
print(summary_stats)

# 1. 個人内平均利他性スコアの算出
individual_altruism <- all_data %>%
  group_by(participant.code) %>%
  summarise(
    mean_altruism = mean(player.payoff_receiver),
    sd_altruism = sd(player.payoff_receiver),
    mean_tech_trust = mean(player.tech_trust),
    mean_ai_satisfaction = mean(player.ai_satisfaction),
    .groups = "drop"
  )

print("Individual Altruism Summary:")
print(summary(individual_altruism))

# データの妥当性チェック
if (nrow(all_data) == 0) {
  stop("No data available after preprocessing")
}

if (n_distinct(all_data$participant.code) < 2) {
  stop("Not enough participants for random effects")
}

# 3. ランダム切片モデルの構築
tryCatch({
  random_intercept_model <- lmer(
    player.payoff_receiver ~ 
      player.equality_preference + 
      player.tech_trust +
      player.ai_satisfaction +
      date +
      (1 | participant.code),
    data = all_data,
    REML = TRUE
  )
  
  print("Random Intercept Model Summary:")
  print(summary(random_intercept_model))
}, error = function(e) {
  cat("Error in random intercept model:", conditionMessage(e), "\n")
})

# 4. ランダム係数モデルへの拡張
tryCatch({
  random_slope_model <- lmer(
    player.payoff_receiver ~ 
      player.equality_preference + 
      player.tech_trust +
      player.ai_satisfaction +
      date +
      (1 + player.equality_preference | participant.code),
    data = all_data,
    REML = TRUE
  )
  
  print("Random Slope Model Summary:")
  print(summary(random_slope_model))
}, error = function(e) {
  cat("Error in random slope model:", conditionMessage(e), "\n")
})

# 以降のコードは、モデルが正常に構築された場合のみ実行
if (exists("random_intercept_model") && exists("random_slope_model")) {
  # 5. モデル比較（尤度比検定）
  model_comparison <- anova(random_intercept_model, random_slope_model)
  
  # 6. 残差の正規性検定とQQプロット
  qq_plot <- ggplot(data.frame(residuals = residuals(random_slope_model)), 
                   aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line() +
    theme_minimal() +
    labs(title = "残差のQQプロット",
         x = "理論分位数",
         y = "標本分位数")
  
  # 7. 個人間変動の可視化（ランダム効果プロット）
  random_effects_plot <- plot_model(random_slope_model, 
                                  type = "re",
                                  show.values = TRUE)
  
  # 8. 効果量の計算（ICC）
  icc_value <- performance::icc(random_slope_model)
  
  # 9. 固定効果の可視化
  fixed_effects_plot <- plot_model(random_slope_model, 
                                 type = "est",
                                 show.values = TRUE)
  
  # 結果の保存
  # プロットの保存
  ggsave("analysis/altruism_analysis/qq_plot.png", qq_plot, width = 8, height = 6)
  ggsave("analysis/altruism_analysis/random_effects.png", random_effects_plot, width = 10, height = 8)
  ggsave("analysis/altruism_analysis/fixed_effects.png", fixed_effects_plot, width = 10, height = 6)
  
  # 結果のMarkdownファイル作成
  sink("analysis/altruism_analysis/analysis_results.md")
  
  cat("# AI条件における利他性スコアの決定要因分析結果\n\n")
  
  cat("## 1. データ概要\n")
  cat("### 1.1 基本統計量\n")
  print(summary_stats)
  
  cat("\n### 1.2 個人内平均利他性スコア\n")
  print(summary(individual_altruism))
  
  cat("\n## 2. モデル分析結果\n")
  cat("### 2.1 ランダム切片モデル\n")
  print(summary(random_intercept_model))
  
  cat("\n### 2.2 ランダム係数モデル\n")
  print(summary(random_slope_model))
  
  cat("\n### 2.3 モデル比較\n")
  print(model_comparison)
  
  cat("\n### 2.4 級内相関係数（ICC）\n")
  print(icc_value)
  
  cat("\n## 3. 主な発見\n")
  cat("1. 平等選好の影響:\n")
  cat("   - 係数と有意性\n")
  cat("   - 個人間変動の大きさ\n")
  cat("2. AIへの態度の影響:\n")
  cat("   - 技術信頼度の効果\n")
  cat("   - AI満足度の効果\n")
  cat("3. 実験日の効果:\n")
  cat("   - 日付による違い\n")
  cat("4. 個人差の大きさ:\n")
  cat("   - ICC値の解釈\n")
  cat("   - ランダム効果の分布\n")
  
  sink()
} 