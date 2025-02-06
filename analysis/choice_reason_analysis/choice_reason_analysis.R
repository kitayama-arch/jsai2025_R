# 必要なパッケージの読み込み
library(tidyverse)
library(stats)
library(car)
library(effectsize)
library(ggplot2)
library(rstatix)

# カレントディレクトリをワークスペースのルートに設定
setwd("../..")

# データの読み込み
# AI条件のデータ
data_0117_3_quest <- read_csv("../../AI2025_data/20250117_3/questionnaire_app_2025-01-17 (1).csv", show_col_types = FALSE)
data_0120_5_quest <- read_csv("../../AI2025_data/20250120_5/questionnaire_app_2025-01-20.csv", show_col_types = FALSE)

# コントロール条件のデータ
data_0117_4_quest <- read_csv("../../AI2025_data/20250117_4/questionnaire_app_2025-01-17 (2).csv", show_col_types = FALSE)
data_0120_4_quest <- read_csv("../../AI2025_data/20250120_4/questionnaire_app_2025-01-20.csv", show_col_types = FALSE)

# データの前処理
# AI条件のデータ結合
ai_data <- bind_rows(
  mutate(data_0117_3_quest, date = "2025-01-17"),
  mutate(data_0120_5_quest, date = "2025-01-20")
) %>%
  mutate(condition = "AI")

# コントロール条件のデータ結合
control_data <- bind_rows(
  mutate(data_0117_4_quest, date = "2025-01-17"),
  mutate(data_0120_4_quest, date = "2025-01-20")
) %>%
  mutate(condition = "Control")

# 全データの結合
survey_data <- bind_rows(ai_data, control_data) %>%
  filter(participant.visited == 1) %>%
  select(
    participant.code,
    condition,
    date,
    player.selfish_preference,
    player.equality_preference,
    player.efficiency_preference,
    player.competitive_preference,
    player.gender,
    player.age,
    player.final_total_payoff
  ) %>%
  rename(
    id = participant.code,
    selfish_preference = player.selfish_preference,
    equality_preference = player.equality_preference,
    efficiency_preference = player.efficiency_preference,
    competitive_preference = player.competitive_preference,
    gender = player.gender,
    age = player.age,
    final_total_payoff = player.final_total_payoff
  )

# 選好変数の正規性検定
shapiro_results <- survey_data %>%
  select(selfish_preference:competitive_preference) %>%
  map(~shapiro.test(.x)) %>%
  map_df(~tibble(
    statistic = .x$statistic,
    p.value = .x$p.value
  ), .id = "preference")

# MANOVAの実行
manova_model <- manova(
  cbind(selfish_preference, equality_preference,
        efficiency_preference, competitive_preference) ~ condition,
  data = survey_data
)

# 事後t検定（Holm補正）
t_test_results <- map_df(
  c("selfish", "equality", "efficiency", "competitive"),
  function(pref) {
    t.test(
      as.formula(paste0(pref, "_preference ~ condition")),
      data = survey_data,
      alternative = "two.sided"
    ) %>%
      broom::tidy() %>%
      mutate(preference = pref)
  }
) %>%
  mutate(p.adjusted = p.adjust(p.value, method = "holm"))

# 効果量の計算
effect_sizes <- map_df(
  c("selfish", "equality", "efficiency", "competitive"),
  function(pref) {
    cohens_d(
      as.formula(paste0(pref, "_preference ~ condition")),
      data = survey_data
    ) %>%
      as.data.frame() %>%
      mutate(preference = pref)
  }
)

# 選好プロファイルの可視化
preference_plot <- survey_data %>%
  pivot_longer(
    cols = c(selfish_preference:competitive_preference),
    names_to = "preference_type",
    values_to = "value"
  ) %>%
  ggplot(aes(x = condition, y = value, fill = condition)) +
  geom_violin(alpha = 0.3) +
  geom_boxplot(width = 0.2, alpha = 0.7) +
  facet_wrap(~preference_type) +
  theme_minimal() +
  labs(
    title = "選好プロファイルの条件間比較",
    x = "条件",
    y = "選好スコア"
  )

# 分析結果を保存するディレクトリに移動
setwd("analysis/choice_reason_analysis")

# 結果の保存
ggsave(
  "preference_profile.png",
  preference_plot,
  width = 10,
  height = 8
)

# 分析結果のまとめ
results_summary <- list(
  shapiro_test = shapiro_results,
  manova_results = summary(manova_model),
  t_test_results = t_test_results,
  effect_sizes = effect_sizes
)

# 結果をRDSファイルとして保存
saveRDS(
  results_summary,
  "analysis_results.rds"
)

# 分析結果をテキストファイルとして保存
sink("analysis_results.txt")
cat("選好変数の正規性検定結果:\n")
print(shapiro_results)
cat("\nMANOVA分析結果:\n")
print(summary(manova_model))
cat("\n事後t検定結果（Holm補正後）:\n")
print(t_test_results)
cat("\n効果量（Cohen's d）:\n")
print(effect_sizes)
sink() 