# 選好間の相互作用の詳細分析
library(tidyverse)
library(ggplot2)
library(viridis)
library(rpart)

# データの読み込み（既存のデータ前処理を使用）
source("choice_reason_detailed.R")

# 1. 非線形関係性の分析 -----------------------------------------

# 多項式回帰による非線形関係の分析
preference_pairs <- list(
  c("efficiency_preference", "equality_preference"),
  c("competitive_preference", "equality_preference"),
  c("competitive_preference", "efficiency_preference")
)

# 非線形関係の可視化の改善
plot_nonlinear_relationship <- function(pair) {
  # データの準備
  plot_data <- survey_data %>%
    select(all_of(c(pair, "condition")))
  
  # 予測値の計算
  pred_data <- expand_grid(
    condition = c("AI", "Control"),
    x = seq(min(plot_data[[pair[2]]]), max(plot_data[[pair[2]]]), length.out = 100)
  ) %>%
    group_by(condition) %>%
    group_modify(~{
      model_data <- filter(plot_data, condition == .y$condition)
      model_formula <- as.formula(paste(pair[1], "~poly(", pair[2], ",2)"))
      model <- lm(model_formula, data = model_data)
      
      new_data <- data.frame(x = .x$x)
      names(new_data) <- pair[2]
      
      mutate(.x, pred = predict(model, newdata = new_data))
    })
  
  # 改善された可視化
  ggplot() +
    geom_point(data = plot_data, 
               aes_string(x = pair[2], y = pair[1], color = "condition"),
               alpha = 0.6, size = 3) +
    geom_line(data = pred_data,
              aes(x = x, y = pred, color = condition),
              size = 1.2) +
    geom_ribbon(data = pred_data,
                aes(x = x, 
                    ymin = pred - sd(plot_data[[pair[1]]]),
                    ymax = pred + sd(plot_data[[pair[1]]]),
                    fill = condition),
                alpha = 0.2) +
    theme_minimal() +
    scale_color_viridis_d(name = "条件") +
    scale_fill_viridis_d(name = "条件") +
    labs(title = paste("選好間の非線形関係:", 
                      gsub("_preference", "", pair[2]), "vs", 
                      gsub("_preference", "", pair[1])),
         subtitle = "条件別の傾向と95%信頼区間",
         x = gsub("_preference", "選好", pair[2]),
         y = gsub("_preference", "選好", pair[1])) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      legend.title = element_text(size = 10),
      legend.position = "right"
    )
}

# 2. 条件付き効果の分析 -----------------------------------------

# 選好の閾値による分類
threshold_analysis <- survey_data %>%
  mutate(
    selfish_level = case_when(
      selfish_preference >= quantile(selfish_preference, 0.75) ~ "high",
      selfish_preference <= quantile(selfish_preference, 0.25) ~ "low",
      TRUE ~ "medium"
    )
  ) %>%
  group_by(condition, selfish_level) %>%
  summarise(
    n = n(),
    equality_mean = mean(equality_preference),
    efficiency_mean = mean(efficiency_preference),
    competitive_mean = mean(competitive_preference),
    equality_sd = sd(equality_preference),
    efficiency_sd = sd(efficiency_preference),
    competitive_sd = sd(competitive_preference),
    .groups = "drop"
  )

# 2. 条件付き効果の新しい可視化 ---------------------------------------
conditional_effect_plot <- threshold_analysis %>%
  pivot_longer(
    cols = c(equality_mean, efficiency_mean, competitive_mean),
    names_to = "preference_type",
    values_to = "mean"
  ) %>%
  mutate(
    preference_type = factor(preference_type,
                           levels = c("equality_mean", "efficiency_mean", "competitive_mean"),
                           labels = c("平等性", "効率性", "競争性"))
  ) %>%
  ggplot(aes(x = preference_type, y = mean, fill = selfish_level)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~condition) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    title = "利己的選好レベルによる他の選好への影響",
    subtitle = "条件別の比較",
    x = "選好タイプ",
    y = "平均スコア",
    fill = "利己的選好レベル"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# 3. 競争性閾値効果の可視化 ------------------------------------------
threshold_effect_plot <- survey_data %>%
  mutate(
    competition_level = case_when(
      competitive_preference >= 6.5 ~ "高",
      competitive_preference >= 4.5 ~ "中",
      TRUE ~ "低"
    ),
    competition_level = factor(competition_level, levels = c("低", "中", "高"))
  ) %>%
  ggplot(aes(x = competition_level, y = equality_preference, fill = condition)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(
    title = "競争性レベルごとの平等性選好",
    subtitle = "6.5を超える高競争性での特徴的な効果",
    x = "競争性レベル",
    y = "平等性選好",
    fill = "条件"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

# 4. 選好の組み合わせ効果 ---------------------------------------

# 選好の組み合わせパターンの分析
preference_patterns <- survey_data %>%
  mutate(
    across(
      c(efficiency_preference, competitive_preference),
      ~ case_when(
        . >= median(.) ~ "high",
        TRUE ~ "low"
      ),
      .names = "{.col}_level"
    )
  ) %>%
  group_by(condition, efficiency_preference_level, competitive_preference_level) %>%
  summarise(
    n = n(),
    equality_mean = mean(equality_preference),
    equality_sd = sd(equality_preference),
    .groups = "drop"
  )

# 決定木による選好パターンの分析
tree_model <- rpart(
  equality_preference ~ efficiency_preference + competitive_preference + condition,
  data = survey_data,
  control = rpart.control(maxdepth = 3, minsplit = 10)
)

# 結果の保存
sink("preference_interaction_results.txt")

cat("1. 非線形関係性分析:\n\n")
for (pair in preference_pairs) {
  cat(paste("\n", pair[1], "vs", pair[2], ":\n"))
  model_ai <- lm(as.formula(paste(pair[1], "~poly(", pair[2], ",2)")), 
                 data = filter(survey_data, condition == "AI"))
  model_control <- lm(as.formula(paste(pair[1], "~poly(", pair[2], ",2)")), 
                     data = filter(survey_data, condition == "Control"))
  
  cat("AI条件:\n")
  print(summary(model_ai))
  cat("\nControl条件:\n")
  print(summary(model_control))
}

cat("\n\n2. 条件付き効果分析:\n\n")
cat("利己的選好レベルごとの他の選好の平均値:\n")
print(threshold_analysis)

cat("\n\n3. 選好の組み合わせ効果:\n\n")
cat("効率性×競争性の組み合わせパターン:\n")
print(preference_patterns)

cat("\n\n決定木分析結果:\n")
print(tree_model)

sink()

# 可視化の保存
for (pair in preference_pairs) {
  ggsave(
    paste0("nonlinear_", paste(pair, collapse="_"), ".pdf"),
    plot_nonlinear_relationship(pair),
    width = 8, height = 6, dpi = 300, bg = "white"
  )
}

# 選好パターンの可視化
pattern_plot <- ggplot(preference_patterns, 
                      aes(x = efficiency_preference_level, 
                          y = equality_mean, 
                          fill = competitive_preference_level)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = equality_mean - equality_sd,
                    ymax = equality_mean + equality_sd),
                position = position_dodge(0.9),
                width = 0.2) +
  facet_wrap(~condition) +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = "効率性×競争性の組み合わせによる平等性選好",
       x = "効率性選好レベル",
       y = "平均平等性選好",
       fill = "競争的選好レベル")

ggsave("preference_patterns.pdf", pattern_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# 4. 決定木の可視化 -------------------------------------------------
library(rpart.plot)

# 日本語フォントの設定
par(family = "HiraKakuProN-W3")

# 決定木の可視化
pdf("decision_tree.pdf", width = 12, height = 8)
rpart.plot(tree_model,
           type = 2,  # ノードの形状を変更
           extra = 1,  # ノードの観測数を表示
           box.palette = "GnBu",  # 色パレット
           branch.lty = 3,  # 枝線のスタイル
           shadow.col = "gray",  # 影の色
           nn = TRUE,  # ノード番号を表示
           fallen.leaves = TRUE,  # 末端ノードを下部に揃える
           main = "平等性選好を予測する決定木分析",
           sub = "競争性と効率性の閾値効果",
           tweak = 1.2,  # テキストサイズの調整
           digits = 3,  # 小数点以下3桁まで表示
           split.prefix = "≥",  # 分岐の条件表示
           split.suffix = "",
           branch = 0.3,  # 枝の長さ
           compress = TRUE)  # ツリーを圧縮
dev.off()

# 決定木の詳細な結果をテキストファイルに保存
sink("decision_tree_details.txt")
cat("決定木分析の詳細結果:\n\n")
print(tree_model)
cat("\n\nノードごとの詳細情報:\n")
print(summary(tree_model))
cat("\n\n変数重要度:\n")
print(tree_model$variable.importance)
sink()

# 全ての図を1ページにまとめた概要図の作成
summary_plot <- (conditional_effect_plot + threshold_effect_plot) /
  plot_spacer() +
  plot_annotation(
    title = "選好間相互作用の主要な発見",
    subtitle = "条件付き効果と閾値効果の可視化",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  )

ggsave("preference_interaction_summary.pdf", 
       summary_plot, 
       width = 15, height = 10) 