# 必要なパッケージの読み込み
library(tidyverse)
library(lme4)
library(car)
library(ggplot2)
library(knitr)
library(rmarkdown)
library(gridExtra)

# データの読み込み
# AI条件のデータ
data_0117_3_dict <- read_csv("../../AI2025_data/20250117_3/dictator_app_2025-01-17.csv")
data_0117_3_quest <- read_csv("../../AI2025_data/20250117_3/questionnaire_app_2025-01-17 (1).csv")
data_0120_5_dict <- read_csv("../../AI2025_data/20250120_5/dictator_app_2025-01-20.csv")
data_0120_5_quest <- read_csv("../../AI2025_data/20250120_5/questionnaire_app_2025-01-20.csv")

# コントロール条件のデータ
data_0117_4_dict <- read_csv("../../AI2025_data/20250117_4/Base_dictator_app_2025-01-17.csv")
data_0117_4_quest <- read_csv("../../AI2025_data/20250117_4/questionnaire_app_2025-01-17 (2).csv")
data_0120_4_dict <- read_csv("../../AI2025_data/20250120_4/Base_dictator_app_2025-01-20.csv")
data_0120_4_quest <- read_csv("../../AI2025_data/20250120_4/questionnaire_app_2025-01-20.csv")

# データのクリーニングと前処理
clean_and_merge_data <- function(dict_data, quest_data, condition) {
  # 実際に参加した参加者のデータのみを抽出
  dict_clean <- dict_data %>%
    filter(participant.visited == 1) %>%
    # コントロール条件の場合、ラウンド16を除外
    {if(condition == "Control") 
      filter(., subsession.round_number != 16) 
     else .}
  
  quest_clean <- quest_data %>%
    filter(participant.visited == 1)
  
  # 選好データの抽出と標準化
  preferences <- quest_clean %>%
    select(participant.code,
           player.selfish_preference,
           player.equality_preference,
           player.efficiency_preference,
           player.competitive_preference,
           player.gender,
           player.age) %>%
    mutate(across(starts_with("player.") & contains("preference"), scale))
  
  # 報酬データの処理
  payoffs <- dict_clean %>%
    select(participant.code,
           subsession.round_number,
           player.payoff,     # 報酬額
           player.choice) %>% # 選択も一応残す
    mutate(
      condition = condition,
      player.payoff = as.numeric(player.payoff)  # 数値型に変換
    )
  
  # データのマージ
  merged_data <- payoffs %>%
    left_join(preferences, by = "participant.code")
  
  return(merged_data)
}

# データの処理
ai_data_0117 <- clean_and_merge_data(data_0117_3_dict, data_0117_3_quest, "AI")
ai_data_0120 <- clean_and_merge_data(data_0120_5_dict, data_0120_5_quest, "AI")
control_data_0117 <- clean_and_merge_data(data_0117_4_dict, data_0117_4_quest, "Control")
control_data_0120 <- clean_and_merge_data(data_0120_4_dict, data_0120_4_quest, "Control")

# 全データの結合
all_data <- bind_rows(
  ai_data_0117,
  ai_data_0120,
  control_data_0117,
  control_data_0120
) %>%
  mutate(condition = factor(condition, levels = c("Control", "AI")))

# 選好タイプごとのプロット作成関数
create_preference_plot <- function(data, preference_var, title_ja) {
  ggplot(data, 
         aes_string(x = preference_var, 
                    y = "player.payoff", 
                    color = "condition")) +
    geom_smooth(method = "lm") +
    geom_point(alpha = 0.1) +  # 個別データ点を薄く表示
    facet_wrap(~condition) +
    labs(x = paste0(title_ja, " (標準化)"),
         y = "報酬額",
         title = paste0(title_ja, "と報酬額の関係"),
         color = "条件") +
    theme_minimal() +
    theme(text = element_text(family = "HiraKakuProN-W3"))
}

# 各選好タイプのプロット作成
p1 <- create_preference_plot(all_data, "player.selfish_preference", "利己的選好")
p2 <- create_preference_plot(all_data, "player.equality_preference", "平等性選好")
p3 <- create_preference_plot(all_data, "player.efficiency_preference", "効率性選好")
p4 <- create_preference_plot(all_data, "player.competitive_preference", "競争的選好")

# プロットの保存
ggsave("payoff_preference_plots.png", 
       arrangeGrob(p1, p2, p3, p4, ncol=2), 
       width = 15, height = 12)

# 結果のMarkdownファイル作成
sink("payoff_analysis_results.md")
cat("# 選好と報酬額の関係性分析結果\n\n")

cat("## 1. データ概要\n")
cat("### 1.1 基本統計量\n")
print(summary(all_data))

cat("\n### 1.2 条件ごとの報酬額の要約統計量\n")
print(all_data %>%
  group_by(condition) %>%
  summarise(
    mean_payoff = mean(player.payoff),
    sd_payoff = sd(player.payoff),
    min_payoff = min(player.payoff),
    max_payoff = max(player.payoff)
  ))

# まず通常の線形モデルで分析
model_lm <- lm(
  player.payoff ~ player.selfish_preference + player.equality_preference + 
    player.efficiency_preference + player.competitive_preference + 
    condition + player.gender + player.age,
  data = all_data
)

# 交互作用を含むモデル
model_interaction <- lm(
  player.payoff ~ player.selfish_preference * condition + 
    player.equality_preference * condition + 
    player.efficiency_preference * condition + 
    player.competitive_preference * condition + 
    player.gender + player.age,
  data = all_data
)

cat("\n## 2. 選好と報酬額の関係性分析\n")
cat("### 2.1 基本モデル概要\n")
print(summary(model_lm))

cat("\n### 2.2 交互作用モデル概要\n")
print(summary(model_interaction))

cat("\n### 2.3 多重共線性チェック\n")
print(vif(model_lm))
print(vif(model_interaction))

# 交互作用の可視化
interaction_plots <- list()

# 各選好タイプについて交互作用プロットを作成
preference_vars <- c(
  "player.selfish_preference" = "利己的選好",
  "player.equality_preference" = "平等性選好",
  "player.efficiency_preference" = "効率性選好",
  "player.competitive_preference" = "競争的選好"
)

for (var in names(preference_vars)) {
  interaction_plots[[var]] <- ggplot(all_data, 
    aes_string(x = var, y = "player.payoff", color = "condition")) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      x = paste0(preference_vars[var], " (標準化)"),
      y = "報酬額",
      title = paste0(preference_vars[var], "と条件の交互作用"),
      color = "条件"
    ) +
    theme_minimal() +
    theme(text = element_text(family = "HiraKakuProN-W3"))
}

# 交互作用プロットの保存
ggsave("preference_condition_interactions.png", 
       arrangeGrob(grobs = interaction_plots, ncol = 2), 
       width = 15, height = 12)

cat("\n## 3. 分析結果の解釈\n\n")

cat("### 3.1 選好タイプごとの影響\n")
coef_summary <- summary(model_lm)$coefficients

cat("1. 利己的選好:\n")
cat("   - 係数: ", coef_summary["player.selfish_preference", "Estimate"], "\n")
cat("   - p値: ", coef_summary["player.selfish_preference", "Pr(>|t|)"], "\n")

cat("\n2. 平等性選好:\n")
cat("   - 係数: ", coef_summary["player.equality_preference", "Estimate"], "\n")
cat("   - p値: ", coef_summary["player.equality_preference", "Pr(>|t|)"], "\n")

cat("\n3. 効率性選好:\n")
cat("   - 係数: ", coef_summary["player.efficiency_preference", "Estimate"], "\n")
cat("   - p値: ", coef_summary["player.efficiency_preference", "Pr(>|t|)"], "\n")

cat("\n4. 競争的選好:\n")
cat("   - 係数: ", coef_summary["player.competitive_preference", "Estimate"], "\n")
cat("   - p値: ", coef_summary["player.competitive_preference", "Pr(>|t|)"], "\n")

cat("\n### 3.2 条件の影響\n")
cat("- AI条件の効果: ", coef_summary["conditionAI", "Estimate"], "\n")
cat("- p値: ", coef_summary["conditionAI", "Pr(>|t|)"], "\n")

cat("\n### 3.3 主な発見\n")
cat("1. 選好と報酬額の関係:\n")
cat("   - 利己的選好の影響\n")
cat("   - その他の選好タイプの影響\n")
cat("2. 条件による違い:\n")
cat("   - AI条件とControl条件の報酬額の差\n")
cat("3. 個人差:\n")
cat("   - 年齢や性別の影響\n")

sink() 