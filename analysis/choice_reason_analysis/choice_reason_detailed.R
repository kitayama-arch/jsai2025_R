# 必要なパッケージの読み込み
library(tidyverse)
library(stats)
library(cluster)
library(factoextra)
library(corrplot)
library(psych)
library(ggplot2)
library(scales)
library(viridis)
library(patchwork)

# データの読み込み（元のスクリプトと同じ前処理を使用）
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

# 1. 選好間の相関構造分析 ----------------------------------------

# 条件別の相関行列計算
correlation_analysis <- function(data) {
  preference_cols <- c("selfish_preference", "equality_preference",
                      "efficiency_preference", "competitive_preference")
  cor_matrix <- cor(data[preference_cols], method = "spearman")
  return(cor_matrix)
}

# AI条件の相関行列
ai_cor <- survey_data %>%
  filter(condition == "AI") %>%
  correlation_analysis()

# Control条件の相関行列
control_cor <- survey_data %>%
  filter(condition == "Control") %>%
  correlation_analysis()

# 相関係数の差の検定
test_cor_diff <- function(r1, r2, n1, n2) {
  z1 <- atanh(r1)
  z2 <- atanh(r2)
  se <- sqrt(1/(n1-3) + 1/(n2-3))
  z <- (z1 - z2)/se
  p <- 2 * (1 - pnorm(abs(z)))
  return(c(z_stat = z, p_value = p))
}

# 相関係数の差の検定を実行
n_ai <- sum(survey_data$condition == "AI")
n_control <- sum(survey_data$condition == "Control")

# 効率性-平等性の相関の差を検定
efficiency_equality_test <- test_cor_diff(
  ai_cor["efficiency_preference", "equality_preference"],
  control_cor["efficiency_preference", "equality_preference"],
  n_ai, n_control
)

# 競争的-平等性の相関の差を検定
competitive_equality_test <- test_cor_diff(
  ai_cor["competitive_preference", "equality_preference"],
  control_cor["competitive_preference", "equality_preference"],
  n_ai, n_control
)

# 競争的-効率性の相関の差を検定
competitive_efficiency_test <- test_cor_diff(
  ai_cor["competitive_preference", "efficiency_preference"],
  control_cor["competitive_preference", "efficiency_preference"],
  n_ai, n_control
)

# 2. 選好の一貫性分析 ---------------------------------------------

# 選好の一貫性スコアの計算（標準偏差の逆数）
consistency_scores <- survey_data %>%
  group_by(condition) %>%
  summarise(
    selfish_consistency = 1/sd(selfish_preference),
    equality_consistency = 1/sd(equality_preference),
    efficiency_consistency = 1/sd(efficiency_preference),
    competitive_consistency = 1/sd(competitive_preference),
    .groups = "drop"
  )

# 選好ごとの条件間比較
preference_comparison <- survey_data %>%
  pivot_longer(
    cols = c(selfish_preference, equality_preference, 
             efficiency_preference, competitive_preference),
    names_to = "preference_type",
    values_to = "score"
  ) %>%
  group_by(preference_type) %>%
  summarise(
    ai_mean = mean(score[condition == "AI"]),
    control_mean = mean(score[condition == "Control"]),
    ai_sd = sd(score[condition == "AI"]),
    control_sd = sd(score[condition == "Control"]),
    t_stat = t.test(score ~ condition)$statistic,
    p_value = t.test(score ~ condition)$p.value,
    .groups = "drop"
  )

# 3. 個人特性との交互作用分析 ------------------------------------

# 性別と条件の交互作用
gender_interaction <- survey_data %>%
  pivot_longer(
    cols = c(selfish_preference, equality_preference, 
             efficiency_preference, competitive_preference),
    names_to = "preference_type",
    values_to = "score"
  ) %>%
  group_by(preference_type) %>%
  summarise(
    f_value = summary(aov(score ~ condition * gender))[[1]]$"F value"[3],
    p_value = summary(aov(score ~ condition * gender))[[1]]$"Pr(>F)"[3],
    .groups = "drop"
  )

# 年齢と条件の交互作用
age_interaction <- survey_data %>%
  pivot_longer(
    cols = c(selfish_preference, equality_preference, 
             efficiency_preference, competitive_preference),
    names_to = "preference_type",
    values_to = "score"
  ) %>%
  group_by(preference_type) %>%
  summarise(
    t_stat = summary(lm(score ~ condition * age))$coefficients[4, 3],
    p_value = summary(lm(score ~ condition * age))$coefficients[4, 4],
    .groups = "drop"
  )

# 4. 結果の保存 -------------------------------------------------

# 相関分析結果をテキストファイルとして保存
sink("analysis_results.txt")

cat("1. 選好間相関の条件間比較:\n\n")
cat("効率性-平等性の相関:\n")
cat("AI条件での相関:", ai_cor["efficiency_preference", "equality_preference"], "\n")
cat("Control条件での相関:", control_cor["efficiency_preference", "equality_preference"], "\n")
cat("Z統計量:", efficiency_equality_test["z_stat"], "\n")
cat("p値:", efficiency_equality_test["p_value"], "\n\n")

cat("競争的-平等性の相関:\n")
cat("AI条件での相関:", ai_cor["competitive_preference", "equality_preference"], "\n")
cat("Control条件での相関:", control_cor["competitive_preference", "equality_preference"], "\n")
cat("Z統計量:", competitive_equality_test["z_stat"], "\n")
cat("p値:", competitive_equality_test["p_value"], "\n\n")

cat("競争的-効率性の相関:\n")
cat("AI条件での相関:", ai_cor["competitive_preference", "efficiency_preference"], "\n")
cat("Control条件での相関:", control_cor["competitive_preference", "efficiency_preference"], "\n")
cat("Z統計量:", competitive_efficiency_test["z_stat"], "\n")
cat("p値:", competitive_efficiency_test["p_value"], "\n\n")

cat("\n2. 選好の条件間比較:\n\n")
print(preference_comparison)

cat("\n3. 個人特性との交互作用:\n\n")
cat("性別との交互作用:\n")
print(gender_interaction)
cat("\n年齢との交互作用:\n")
print(age_interaction)

sink()

# 相関行列の可視化
pdf("correlation_matrices.pdf", width = 10, height = 5)
par(mfrow = c(1,2))
corrplot(ai_cor, method = "color", type = "upper",
         addCoef.col = "black", title = "AI条件での選好間相関")
corrplot(control_cor, method = "color", type = "upper",
         addCoef.col = "black", title = "Control条件での選好間相関")
dev.off()

# 2. クラスター分析 ---------------------------------------------

# データの標準化
scaled_preferences <- survey_data %>%
  select(selfish_preference:competitive_preference) %>%
  scale()

# 最適なクラスター数の決定
wss <- sapply(1:10, function(k) {
  kmeans(scaled_preferences, centers = k, nstart = 25)$tot.withinss
})

# エルボープロットの作成
elbow_plot <- fviz_nbclust(scaled_preferences, kmeans, method = "wss") +
  theme_minimal() +
  labs(title = "最適クラスター数の決定（エルボー法）",
       x = "クラスター数",
       y = "群内平方和")

# k-means クラスタリング（最適なkを使用）
k <- 3  # エルボープロットから決定
kmeans_result <- kmeans(scaled_preferences, centers = k, nstart = 25)

# クラスターの特徴付け
cluster_profiles <- bind_cols(
  survey_data,
  cluster = factor(kmeans_result$cluster)
) %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(
      c(selfish_preference:competitive_preference),
      list(mean = mean, sd = sd)
    ),
    ai_proportion = mean(condition == "AI")
  )

# クラスター分布の可視化
cluster_plot <- fviz_cluster(kmeans_result, data = scaled_preferences,
                           geom = "point",
                           ellipse.type = "convex",
                           ggtheme = theme_minimal()) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(title = "選好パターンのクラスター分布",
       x = "第1主成分",
       y = "第2主成分")

# 3. クラスター×条件の分析 --------------------------------------

# クラスターと条件のクロス集計表を作成
cluster_data <- bind_cols(
  survey_data,
  cluster = factor(kmeans_result$cluster)
)

# クラスター×条件のクロス集計
contingency_table <- table(cluster_data$cluster, cluster_data$condition)

# カイ二乗検定
chi_sq_test <- chisq.test(contingency_table)

# 各クラスターにおける条件別の選好プロファイル
cluster_profiles_by_condition <- cluster_data %>%
  group_by(cluster, condition) %>%
  summarise(
    n = n(),
    selfish_mean = mean(selfish_preference),
    equality_mean = mean(equality_preference),
    efficiency_mean = mean(efficiency_preference),
    competitive_mean = mean(competitive_preference),
    selfish_sd = sd(selfish_preference),
    equality_sd = sd(equality_preference),
    efficiency_sd = sd(efficiency_preference),
    competitive_sd = sd(competitive_preference),
    .groups = "drop"
  )

# 各クラスター内での条件間の選好差のt検定
preference_tests <- cluster_data %>%
  group_by(cluster) %>%
  summarise(
    selfish_t = t.test(selfish_preference ~ condition)$p.value,
    equality_t = t.test(equality_preference ~ condition)$p.value,
    efficiency_t = t.test(efficiency_preference ~ condition)$p.value,
    competitive_t = t.test(competitive_preference ~ condition)$p.value,
    .groups = "drop"
  )

# クラスター別の選好プロファイルの可視化
preference_plot <- cluster_data %>%
  group_by(cluster, condition) %>%
  summarise(
    n = n(),
    selfish_mean = mean(selfish_preference),
    equality_mean = mean(equality_preference),
    efficiency_mean = mean(efficiency_preference),
    competitive_mean = mean(competitive_preference),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(selfish_mean, equality_mean, efficiency_mean, competitive_mean),
    names_to = "preference_type",
    values_to = "mean"
  ) %>%
  mutate(
    preference_type = str_remove(preference_type, "_mean"),
    cluster = factor(cluster, labels = c("バランス型", "効率重視型", "利己優先型"))
  ) %>%
  ggplot(aes(x = preference_type, y = mean, fill = condition)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_wrap(~cluster) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    title = "クラスター別・条件別の選好プロファイル",
    x = "選好タイプ",
    y = "平均スコア",
    fill = "条件"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 結果の保存
saveRDS(list(
  correlation = list(
    ai = ai_cor,
    control = control_cor
  ),
  clustering = list(
    kmeans_result = kmeans_result,
    profiles = cluster_profiles
  ),
  cluster_comparison = list(
    contingency = contingency_table,
    chi_sq = chi_sq_test,
    profiles = cluster_profiles_by_condition,
    tests = preference_tests
  )
), "detailed_analysis_results.rds")

# 結果の保存
ggsave("elbow_plot.pdf", elbow_plot, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("cluster_plot.pdf", cluster_plot, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("cluster_preference_comparison.pdf", preference_plot, width = 12, height = 6, dpi = 300, bg = "white")

# クラスター分析結果をテキストファイルに追記
sink("analysis_results.txt", append = TRUE)
cat("\n4. クラスター分析結果:\n\n")
cat("クラスタープロファイル:\n")
print(cluster_profiles)

# クラスターと条件のクロス集計
cluster_condition_table <- table(cluster_data$cluster, cluster_data$condition)
cat("\nクラスター×条件のクロス集計:\n")
print(cluster_condition_table)

# カイ二乗検定
chi_sq_test <- chisq.test(cluster_condition_table)
cat("\nカイ二乗検定結果:\n")
print(chi_sq_test)

sink() 