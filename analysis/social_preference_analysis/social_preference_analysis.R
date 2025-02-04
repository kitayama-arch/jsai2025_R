# 必要なパッケージの読み込み
library(tidyverse)
library(stats)
library(lme4)
library(car)
library(emmeans)
library(optimx)

# エラーハンドリング関数
handle_data_loading <- function(file_path) {
  tryCatch({
    data <- read_csv(file_path)
    if (nrow(data) == 0) {
      stop("データが空です: ", file_path)
    }
    return(data)
  }, error = function(e) {
    stop("データの読み込みエラー: ", file_path, "\n", e$message)
  })
}

# データの読み込み
# AI条件の独裁者ゲームデータ
data_0117_3 <- handle_data_loading("AI2025_data/20250117_3/dictator_app_2025-01-17.csv")
data_0120_5 <- handle_data_loading("AI2025_data/20250120_5/dictator_app_2025-01-20.csv")
# コントロール条件の独裁者ゲームデータ
data_0117_4 <- handle_data_loading("AI2025_data/20250117_4/Base_dictator_app_2025-01-17.csv")
data_0120_4 <- handle_data_loading("AI2025_data/20250120_4/Base_dictator_app_2025-01-20.csv")

# データクリーニングと前処理
clean_dictator_data <- function(data, is_control = FALSE) {
  cleaned_data <- data %>%
    filter(participant.visited == 1) %>%
    filter(!is.na(player.payoff_dictator),
           !is.na(player.payoff_receiver),
           player.payoff_dictator > 0,
           player.payoff_receiver > 0)
  
  if (is_control) {
    cleaned_data <- cleaned_data %>%
      filter(subsession.round_number != 16)
  }
  
  # データの妥当性チェック
  if (nrow(cleaned_data) == 0) {
    stop("クリーニング後のデータが空です")
  }
  
  return(cleaned_data)
}

# データのクリーニング
data_0117_3_clean <- clean_dictator_data(data_0117_3)
data_0120_5_clean <- clean_dictator_data(data_0120_5)
data_0117_4_clean <- clean_dictator_data(data_0117_4, is_control = TRUE)
data_0120_4_clean <- clean_dictator_data(data_0120_4, is_control = TRUE)

# データの統合と変数作成
create_analysis_dataset <- function(data, condition) {
  processed_data <- data %>%
    mutate(
      condition = condition,
      s = as.integer(player.payoff_receiver > player.payoff_dictator),
      r = as.integer(player.payoff_dictator > player.payoff_receiver),
      total_payoff = player.payoff_dictator + player.payoff_receiver,
      diff_payoff = player.payoff_dictator - player.payoff_receiver,
      choice_X = case_when(
        player.choice == "X" ~ TRUE,
        player.choice == "Y" ~ FALSE,
        TRUE ~ NA
      )
    ) %>%
    group_by(participant.code) %>%
    mutate(
      choice_prop = mean(choice_X, na.rm = TRUE),
      total_rounds = n()
    ) %>%
    ungroup() %>%
    filter(total_rounds >= 10)  # 最低10ラウンド以上のデータを持つ参加者のみ
  
  # データの状態を確認
  print(paste("条件:", condition))
  print(paste("参加者数:", n_distinct(processed_data$participant.code)))
  print(paste("総観測数:", nrow(processed_data)))
  print("選択比率の要約:")
  print(summary(processed_data$choice_prop))
  
  return(processed_data)
}

# AI条件とコントロール条件のデータ統合
data_ai <- bind_rows(
  create_analysis_dataset(data_0117_3_clean, "AI"),
  create_analysis_dataset(data_0120_5_clean, "AI")
)

data_control <- bind_rows(
  create_analysis_dataset(data_0117_4_clean, "Control"),
  create_analysis_dataset(data_0120_4_clean, "Control")
)

data_all <- bind_rows(data_ai, data_control)

# 社会的選好パラメータの推定のための尤度関数
likelihood_function <- function(params, data) {
  alpha <- params[1]
  beta <- params[2]
  lambda <- 1  # 選択の感度パラメータ（固定）
  
  # 効用関数の計算
  utility_diff <- with(data, {
    dictator_diff <- player.payoff_dictator * (choice_X - (1 - choice_X))
    receiver_diff <- player.payoff_receiver * (choice_X - (1 - choice_X))
    
    base_utility <- dictator_diff
    inequality_disutility <- alpha * s * receiver_diff + beta * r * receiver_diff
    
    (base_utility + inequality_disutility) / 100  # スケーリング
  })
  
  # ロジット選択確率
  prob <- 1 / (1 + exp(-lambda * utility_diff))
  prob <- pmin(pmax(prob, .Machine$double.eps), 1 - .Machine$double.eps)
  
  # 対数尤度の計算
  log_likelihood <- sum(data$choice_X * log(prob) + (1 - data$choice_X) * log(1 - prob), na.rm = TRUE)
  
  if (!is.finite(log_likelihood)) {
    return(.Machine$double.xmax)
  }
  
  return(-log_likelihood)
}

# パラメータ推定関数の改善
estimate_social_preferences <- function(data) {
  # データの前処理
  clean_data <- data %>%
    filter(!is.na(choice_X),
           !is.na(player.payoff_dictator),
           !is.na(player.payoff_receiver)) %>%
    group_by(participant.code) %>%
    mutate(
      round_normalized = scale(subsession.round_number),
      payoff_diff = player.payoff_dictator - player.payoff_receiver,
      payoff_sum = player.payoff_dictator + player.payoff_receiver,
      payoff_ratio = player.payoff_dictator / player.payoff_receiver
    ) %>%
    ungroup()
  
  print(paste("前処理後の参加者数:", n_distinct(clean_data$participant.code)))
  print(paste("前処理後の観測数:", nrow(clean_data)))
  
  if (nrow(clean_data) == 0) {
    stop("有効なデータがありません")
  }
  
  # 複数の初期値での推定
  start_points <- list(
    c(0.2, 0.2),
    c(0.4, 0.4),
    c(0.6, 0.6),
    c(0.3, 0.5),
    c(0.5, 0.3)
  )
  
  results <- lapply(start_points, function(start) {
    tryCatch({
      result <- optim(
        par = start,
        fn = likelihood_function,
        data = clean_data,
        method = "L-BFGS-B",
        lower = c(0, 0),
        upper = c(0.99, 0.99),  # 境界値を避ける
        control = list(
          maxit = 2000,
          factr = 1e7,
          pgtol = 1e-5
        )
      )
      
      if (result$convergence != 0) {
        return(NULL)
      }
      
      # 結果の妥当性チェック
      if (!all(is.finite(result$par)) || 
          any(result$par < 0) || 
          any(result$par > 1)) {
        return(NULL)
      }
      
      return(result)
    }, error = function(e) NULL)
  })
  
  # 最良の結果を選択
  valid_results <- results[!sapply(results, is.null)]
  if (length(valid_results) == 0) {
    warning("全ての最適化が失敗しました")
    return(list(
      par = c(NA, NA),
      value = NA,
      convergence = -1,
      se = c(NA, NA)
    ))
  }
  
  values <- sapply(valid_results, function(x) x$value)
  best_index <- which.min(values)
  result <- valid_results[[best_index]]
  
  # ブートストラップによる信頼区間の計算
  n_bootstrap <- 1000
  bootstrap_params <- matrix(NA, nrow = n_bootstrap, ncol = 2)
  
  for (i in 1:n_bootstrap) {
    bootstrap_participants <- sample(unique(clean_data$participant.code), 
                                  replace = TRUE)
    bootstrap_data <- clean_data %>%
      filter(participant.code %in% bootstrap_participants)
    
    boot_result <- tryCatch({
      optim(
        par = result$par,
        fn = likelihood_function,
        data = bootstrap_data,
        method = "L-BFGS-B",
        lower = c(0, 0),
        upper = c(0.99, 0.99),
        control = list(
          maxit = 1000,
          factr = 1e7,
          pgtol = 1e-5
        )
      )
    }, error = function(e) NULL)
    
    if (!is.null(boot_result) && boot_result$convergence == 0) {
      bootstrap_params[i,] <- boot_result$par
    }
  }
  
  # 信頼区間の計算
  ci <- apply(bootstrap_params, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  se <- apply(bootstrap_params, 2, sd, na.rm = TRUE)
  
  # 結果の拡張
  result$se <- se
  result$ci <- ci
  return(result)
}

# 条件ごとのパラメータ推定
estimate_by_condition <- function(data) {
  data %>%
    group_by(condition) %>%
    group_modify(~{
      est <- estimate_social_preferences(.x)
      tibble(
        alpha = est$par[1],
        beta = est$par[2],
        alpha_se = est$se[1],
        beta_se = est$se[2],
        convergence = est$convergence,
        log_likelihood = -est$value
      )
    })
}

# 結果の視覚化関数の改善
plot_social_preferences <- function(results) {
  # パラメータプロット
  param_plot <- ggplot(results, aes(x = alpha, y = beta, color = condition)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = beta - beta_se, ymax = beta + beta_se), width = 0.05) +
    geom_errorbarh(aes(xmin = alpha - alpha_se, xmax = alpha + alpha_se), height = 0.05) +
    labs(
      title = "社会的選好パラメータの推定結果",
      x = "α (不利な不平等回避)",
      y = "β (有利な不平等回避)",
      color = "実験条件"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "HiraKakuProN-W3"),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5)
  
  # 選択比率の分布プロット
  choice_plot <- ggplot(data_all, aes(x = condition, y = choice_prop)) +
    geom_boxplot(aes(fill = condition), alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.4) +
    labs(
      title = "条件別の選択比率分布",
      x = "実験条件",
      y = "X選択の比率",
      fill = "実験条件"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "HiraKakuProN-W3"),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )
  
  # 時系列プロット
  time_plot <- ggplot(data_all, aes(x = subsession.round_number, y = as.numeric(choice_X), color = condition)) +
    stat_smooth(method = "loess", se = TRUE) +
    labs(
      title = "ラウンドごとの選択確率の推移",
      x = "ラウンド",
      y = "X選択の確率",
      color = "実験条件"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "HiraKakuProN-W3"),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )
  
  # プロットの保存
  ggsave("analysis/social_preference_analysis/parameter_plot.png", param_plot, width = 8, height = 6)
  ggsave("analysis/social_preference_analysis/choice_distribution.png", choice_plot, width = 8, height = 6)
  ggsave("analysis/social_preference_analysis/time_series.png", time_plot, width = 8, height = 6)
  
  return(list(param_plot = param_plot, choice_plot = choice_plot, time_plot = time_plot))
}

# 統計的検定
conduct_statistical_tests <- function(data, results) {
  # 参加者レベルのデータを作成
  participant_data <- data %>%
    group_by(participant.code, condition) %>%
    summarise(
      choice_prop = mean(choice_X, na.rm = TRUE),
      total_rounds = n(),
      .groups = "drop"
    )
  
  # 条件間の選択比率の差の検定
  choice_test <- wilcox.test(choice_prop ~ condition, data = participant_data)
  
  # 結果の出力
  cat("\n=== 統計的検定結果 ===\n")
  
  cat("\n1. 記述統計:\n")
  summary_stats <- participant_data %>%
    group_by(condition) %>%
    summarise(
      n = n(),
      mean_prop = mean(choice_prop),
      sd_prop = sd(choice_prop),
      median_prop = median(choice_prop),
      .groups = "drop"
    )
  print(summary_stats)
  
  cat("\n2. 選択比率の条件間差 (Wilcoxon検定):\n")
  print(choice_test)
  
  # 結果のまとめ
  test_results <- list(
    choice_test = choice_test,
    summary_stats = summary_stats
  )
  
  return(test_results)
}

# 結果の保存
save_analysis_results <- function(results, test_results, file_path = "analysis/social_preference_analysis/analysis_results.md") {
  sink(file_path)
  
  cat("# 社会的選好パラメータ分析結果\n\n")
  
  cat("## 1. 記述統計\n\n")
  print(test_results$summary_stats)
  
  cat("\n## 2. 統計的検定結果\n\n")
  cat("### 2.1 選択比率の条件間差（Wilcoxon検定）\n")
  print(test_results$choice_test)
  
  cat("\n## 3. 追加の分析\n\n")
  cat("### 3.1 ラウンド効果\n")
  # ラウンド効果の分析を追加
  round_effects <- lmer(choice_X ~ condition + (1|participant.code) + (1|subsession.round_number), 
                       data = data_all)
  print(summary(round_effects))
  
  sink()
}

# 時系列分析の追加
analyze_time_trends <- function(data) {
  # ラウンド効果のモデル
  round_model <- lmer(choice_X ~ condition * scale(subsession.round_number) + 
                       (1 + scale(subsession.round_number)|participant.code),
                     data = data)
  
  # 選択の推移分析
  choice_trends <- data %>%
    group_by(condition, subsession.round_number) %>%
    summarise(
      choice_rate = mean(choice_X, na.rm = TRUE),
      se = sd(choice_X, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    )
  
  # 結果の出力
  cat("\n=== 時系列分析結果 ===\n")
  print(summary(round_model))
  
  # 時系列プロット
  trend_plot <- ggplot(choice_trends, 
                      aes(x = subsession.round_number, 
                          y = choice_rate, 
                          color = condition)) +
    geom_line() +
    geom_ribbon(aes(ymin = choice_rate - se, 
                    ymax = choice_rate + se, 
                    fill = condition), 
                alpha = 0.2) +
    labs(
      title = "ラウンドごとの選択確率の推移",
      x = "ラウンド",
      y = "X選択の確率",
      color = "実験条件",
      fill = "実験条件"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "HiraKakuProN-W3"),
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )
  
  ggsave("analysis/social_preference_analysis/time_trends.png", trend_plot, width = 10, height = 6)
  
  return(list(
    model = round_model,
    trends = choice_trends,
    plot = trend_plot
  ))
}

# メイン実行部分
results <- estimate_by_condition(data_all)
plots <- plot_social_preferences(results)
test_results <- conduct_statistical_tests(data_all, results)
time_analysis <- analyze_time_trends(data_all)
save_analysis_results(results, test_results)

# 結果の表示
print("=== 分析結果 ===")
print(results)
print("\n=== 時系列分析 ===")
print(summary(time_analysis$model))

# 追加の可視化：ラウンドごとの選択パターン
round_pattern_plot <- ggplot(data_all, aes(x = subsession.round_number, fill = condition)) +
  geom_bar(position = "dodge") +
  facet_wrap(~condition) +
  labs(
    title = "ラウンドごとの選択パターン",
    x = "ラウンド",
    y = "選択回数",
    fill = "実験条件"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "HiraKakuProN-W3"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

ggsave("analysis/social_preference_analysis/round_pattern.png", round_pattern_plot, width = 10, height = 6)
