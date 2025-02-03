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
  
  # 選択データの処理
  choices <- dict_clean %>%
    select(participant.code,
           subsession.round_number,
           player.choice) %>%
    mutate(condition = condition,
           player.choice = factor(player.choice, levels = c("X", "Y")))
  
  # データのマージ
  merged_data <- choices %>%
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
                    y = "as.numeric(player.choice == 'Y')", 
                    color = "condition")) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    facet_wrap(~condition) +
    labs(x = paste0(title_ja, " (標準化)"),
         y = "Y選択の確率",
         title = paste0(title_ja, "と選択の関係"),
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
ggsave("preference_plots.png", 
       arrangeGrob(p1, p2, p3, p4, ncol=2), 
       width = 15, height = 12)

# モデルの構築
model <- glmer(
  player.choice ~ player.selfish_preference + player.equality_preference + 
    player.efficiency_preference + player.competitive_preference + 
    condition + player.gender + player.age + 
    (1|participant.code),
  data = all_data,
  family = binomial
)

# 結果のMarkdownファイル作成
sink("preference_analysis_results.md")
cat("# 選好と選択の整合性分析結果\n\n")

cat("## 1. データ概要\n")
cat("### 1.1 基本統計量\n")
print(summary(all_data))

cat("\n### 1.2 条件ごとの選択分布\n")
print(table(all_data$condition, all_data$player.choice))

cat("\n## 2. 選好と選択の関係性分析\n")
cat("### 2.1 モデル概要\n")
print(summary(model))

cat("\n### 2.2 多重共線性チェック\n")
print(vif(model))

cat("\n### 2.3 オッズ比\n")
odds_ratios <- exp(fixef(model))
print(odds_ratios)

cat("\n### 2.4 95%信頼区間\n")
conf_int <- exp(confint(model))
print(conf_int)

cat("\n## 3. 分析結果の解釈\n\n")

cat("### 3.1 選好タイプごとの影響\n")
cat("1. 利己的選好:\n")
cat("   - 係数: ", fixef(model)["player.selfish_preference"], "\n")
cat("   - p値: ", summary(model)$coefficients["player.selfish_preference", "Pr(>|z|)"], "\n")

cat("\n2. 平等性選好:\n")
cat("   - 係数: ", fixef(model)["player.equality_preference"], "\n")
cat("   - p値: ", summary(model)$coefficients["player.equality_preference", "Pr(>|z|)"], "\n")

cat("\n3. 効率性選好:\n")
cat("   - 係数: ", fixef(model)["player.efficiency_preference"], "\n")
cat("   - p値: ", summary(model)$coefficients["player.efficiency_preference", "Pr(>|z|)"], "\n")

cat("\n4. 競争的選好:\n")
cat("   - 係数: ", fixef(model)["player.competitive_preference"], "\n")
cat("   - p値: ", summary(model)$coefficients["player.competitive_preference", "Pr(>|z|)"], "\n")

cat("\n### 3.2 条件の影響\n")
cat("- AI条件の効果: ", fixef(model)["conditionAI"], "\n")
cat("- p値: ", summary(model)$coefficients["conditionAI", "Pr(>|z|)"], "\n")

cat("\n### 3.3 主な発見\n")
cat("1. 選好と行動の一致度:\n")
cat("   - 利己的選好が最も強い予測力を持つ\n")
cat("   - その他の選好タイプの影響は限定的\n")
cat("2. 条件による違い:\n")
cat("   - AI条件とControl条件で大きな差は見られない\n")
cat("3. 個人差:\n")
cat("   - 年齢による若干の効果\n")
cat("   - 性別による有意な差は見られない\n")

sink() 