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
  alpha <- params[1]  # 不利な不平等に対する回避度
  beta <- params[2]   # 有利な不平等に対する回避度
  lambda <- params[3] # 選択の感度パラメータ
  
  # 効用関数の計算（Bruhin et al. 2019の定式化に基づく）
  utility_X <- with(data, {
    # 不平等指標の計算
    s <- as.integer(player.payoff_receiver > player.payoff_dictator)
    r <- as.integer(player.payoff_dictator > player.payoff_receiver)
    
    # 効用関数: u_D(π_D, π_R) = (1 - αs - βr)π_D + (αs + βr)π_R
    (1 - alpha * s - beta * r) * player.payoff_dictator / 1000 + 
    (alpha * s + beta * r) * player.payoff_receiver / 1000
  })

  utility_Y <- with(data, {
    # 選択Yの場合の利得
    payoff_dictator_Y <- 1000 - player.payoff_dictator
    payoff_receiver_Y <- 1000 - player.payoff_receiver
    
    # 不平等指標の計算（選択Yの場合）
    s <- as.integer(payoff_receiver_Y > payoff_dictator_Y)
    r <- as.integer(payoff_dictator_Y > payoff_receiver_Y)
    
    # 効用関数: u_D(π_D, π_R) = (1 - αs - βr)π_D + (αs + βr)π_R
    (1 - alpha * s - beta * r) * payoff_dictator_Y / 1000 + 
    (alpha * s + beta * r) * payoff_receiver_Y / 1000
  })
  
  # 効用差の計算
  utility_diff <- utility_X - utility_Y
  
  # ロジット選択確率の計算
  prob <- 1 / (1 + exp(-lambda * utility_diff))
  
  # 数値的安定性のための調整
  prob <- pmin(pmax(prob, .Machine$double.eps), 1 - .Machine$double.eps)
  
  # 対数尤度の計算
  log_likelihood <- sum(data$choice_X * log(prob) + (1 - data$choice_X) * log(1 - prob), na.rm = TRUE)
  
  # 無限大や非数値をチェック
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
      s = as.integer(player.payoff_receiver > player.payoff_dictator),
      r = as.integer(player.payoff_dictator > player.payoff_receiver),
      total_payoff = player.payoff_dictator + player.payoff_receiver,
      diff_payoff = player.payoff_dictator - player.payoff_receiver,
      choice_prop = mean(choice_X)
    ) %>%
    ungroup()
  
  print(paste("前処理後の参加者数:", n_distinct(clean_data$participant.code)))
  print(paste("前処理後の観測数:", nrow(clean_data)))
  print("\n選択比率の分布:")
  print(summary(clean_data$choice_prop))
  print("\n不平等指標の分布:")
  print("s (不利な不平等):")
  print(table(clean_data$s))
  print("r (有利な不平等):")
  print(table(clean_data$r))
  
  if (nrow(clean_data) == 0) {
    stop("有効なデータがありません")
  }
  
  # より細かいグリッドサーチ
  alpha_grid <- seq(0.1, 0.9, by = 0.1)
  beta_grid <- seq(0.1, 0.9, by = 0.1)
  lambda_grid <- seq(0.5, 5.0, by = 0.5)
  
  grid_results <- expand.grid(alpha = alpha_grid, beta = beta_grid, lambda = lambda_grid)
  grid_results$value <- apply(grid_results, 1, function(params) {
    tryCatch({
      likelihood_function(params, clean_data)
    }, error = function(e) Inf)
  })
  
  # 最良の初期値を選択（上位10個）
  best_starts <- grid_results[order(grid_results$value), ][1:10, 1:3]
  print("\nグリッドサーチの結果（上位10個）:")
  print(best_starts)
  
  # 複数の最適化アルゴリズムを試す
  methods <- c("L-BFGS-B", "Nelder-Mead", "BFGS")
  １all_results <- list()
  
  for (method in methods) {
    for (i in 1:nrow(best_starts)) {
      start <- unlist(best_starts[i, ])
      tryCatch({
        result <- optim(
          par = start,
          fn = likelihood_function,
          data = clean_data,
          method = method,
          lower = c(0.01, 0.01, 0.1),
          upper = c(0.99, 0.99, 10.0),
          control = list(
            maxit = 10000,
            reltol = 1e-8,
            factr = 1e7
          ),
          hessian = TRUE
        )
        
        if (result$convergence == 0) {
          # 標準誤差の計算
          if (all(is.finite(result$hessian)) && det(result$hessian) > 1e-10) {
            se <- sqrt(diag(solve(result$hessian)))
            if (all(se < 1)) {  # 標準誤差が妥当な範囲内かチェック
              result$se <- se
              result$method <- method
              all_results[[length(all_results) + 1]] <- result
            }
          }
        }
      }, error = function(e) NULL)
    }
  }
  
  # 最良の結果を選択
  if (length(all_results) == 0) {
    warning("全ての最適化が失敗しました")
    return(list(
      par = c(NA, NA, NA),
      value = NA,
      convergence = -1,
      se = c(NA, NA, NA)
    ))
  }
  
  # 対数尤度が最大で、かつ標準誤差が妥当な結果を選択
  values <- sapply(all_results, function(x) x$value)
  best_idx <- which.min(values)
  best_result <- all_results[[best_idx]]
  
  # 結果の詳細を表示
  print("\n最適化結果:")
  print(paste("使用したアルゴリズム:", best_result$method))
  print(paste("α =", round(best_result$par[1], 3), "±", round(best_result$se[1], 3)))
  print(paste("β =", round(best_result$par[2], 3), "±", round(best_result$se[2], 3)))
  print(paste("λ =", round(best_result$par[3], 3), "±", round(best_result$se[3], 3)))
  print(paste("対数尤度 =", -best_result$value))
  print(paste("収束状態 =", best_result$convergence))
  
  return(best_result)
}

# ブートストラップによる標準誤差の計算
calculate_bootstrap_se <- function(data, params) {
  n_bootstrap <- 1000
  bootstrap_params <- matrix(NA, nrow = n_bootstrap, ncol = length(params))
  
  for (i in 1:n_bootstrap) {
    # データの再サンプリング
    bootstrap_indices <- sample(1:nrow(data), replace = TRUE)
    bootstrap_data <- data[bootstrap_indices, ]
    
    # パラメータ推定
    result <- tryCatch({
      optim(
        par = params,
        fn = likelihood_function,
        data = bootstrap_data,
        method = "L-BFGS-B",
        lower = c(0.001, 0.001, 0.1),
        upper = c(0.9, 0.9, 3.0),
        control = list(
          maxit = 1000,
          factr = 1e3,
          pgtol = 1e-10
        )
      )
    }, error = function(e) NULL)
    
    if (!is.null(result) && result$convergence == 0) {
      bootstrap_params[i, ] <- result$par
    }
  }
  
  # 標準誤差の計算
  se <- apply(bootstrap_params, 2, sd, na.rm = TRUE)
  return(se)
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
        lambda = est$par[3],
        alpha_se = est$se[1],
        beta_se = est$se[2],
        lambda_se = est$se[3],
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
