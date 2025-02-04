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
# 選択肢データの読み込み
payoff_scenarios <- handle_data_loading("Experiment/payoff_scenarios_analysis.csv")

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
  
  # 選択しなかった方の選択肢データを追加
  cleaned_data <- cleaned_data %>%
    left_join(
      payoff_scenarios %>% 
        filter(Is_Training == TRUE) %>%
        select(
          Game,
          Option_X_Dictator, Option_X_Receiver,
          Option_Y_Dictator, Option_Y_Receiver
        ),
      by = c("subsession.round_number" = "Game")
    ) %>%
    mutate(
      # 選択しなかった方の選択肢のデータを設定
      non_chosen_payoff_dictator = case_when(
        player.choice == "X" ~ Option_Y_Dictator,
        player.choice == "Y" ~ Option_X_Dictator
      ),
      non_chosen_payoff_receiver = case_when(
        player.choice == "X" ~ Option_Y_Receiver,
        player.choice == "Y" ~ Option_X_Receiver
      )
    )
  
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
      total_rounds = n()
    ) %>%
    ungroup() %>%
    filter(total_rounds >= 10)  # 最低10ラウンド以上のデータを持つ参加者のみ
  
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
  
  # 最大金額の計算
  max_payoff <- max(c(data$Option_X_Dictator, data$Option_X_Receiver,
                      data$Option_Y_Dictator, data$Option_Y_Receiver))
  
  # 効用関数の計算（Bruhin et al. 2019の定式化に基づく）
  utility_X <- with(data, {
    # 不平等指標の計算
    s <- as.integer(Option_X_Receiver > Option_X_Dictator)
    r <- as.integer(Option_X_Dictator > Option_X_Receiver)
    
    # 効用関数: u_D(π_D, π_R) = (1 - αs - βr)π_D + (αs + βr)π_R
    (1 - alpha * s - beta * r) * Option_X_Dictator / max_payoff + 
    (alpha * s + beta * r) * Option_X_Receiver / max_payoff
  })

  utility_Y <- with(data, {
    # 不平等指標の計算（選択Yの場合）
    s <- as.integer(Option_Y_Receiver > Option_Y_Dictator)
    r <- as.integer(Option_Y_Dictator > Option_Y_Receiver)
    
    # 効用関数: u_D(π_D, π_R) = (1 - αs - βr)π_D + (αs + βr)π_R
    (1 - alpha * s - beta * r) * Option_Y_Dictator / max_payoff + 
    (alpha * s + beta * r) * Option_Y_Receiver / max_payoff
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
      diff_payoff = player.payoff_dictator - player.payoff_receiver
    ) %>%
    ungroup()
  
  # より広いグリッドサーチ（改善1: より広い範囲）
  alpha_grid <- seq(-0.5, 0.5, by = 0.05)  # 負の値も含める
  beta_grid <- seq(-0.5, 0.5, by = 0.05)   # 負の値も含める
  lambda_grid <- seq(0.1, 15.0, by = 0.5)  # より広い範囲
  
  grid_results <- expand.grid(alpha = alpha_grid, beta = beta_grid, lambda = lambda_grid)
  grid_results$value <- apply(grid_results, 1, function(params) {
    tryCatch({
      likelihood_function(params, clean_data)
    }, error = function(e) Inf)
  })
  
  # 最良の初期値を選択（改善2: より多くの初期値）
  best_starts <- grid_results[order(grid_results$value), ][1:30, 1:3]  # 上位30個に増やす
  
  # より多様な最適化アルゴリズムを試す（改善3）
  methods <- c("L-BFGS-B", "Nelder-Mead", "BFGS", "CG", "SANN")  # SANNを追加
  
  all_results <- list()
  
  for (method in methods) {
    for (i in 1:nrow(best_starts)) {
      start <- unlist(best_starts[i, ])
      tryCatch({
        # SANNの場合は異なるパラメータを使用
        if (method == "SANN") {
          result <- optim(
            par = start,
            fn = likelihood_function,
            data = clean_data,
            method = method,
            control = list(
              maxit = 50000,
              temp = 10,
              tmax = 10
            )
          )
        } else {
          result <- optim(
            par = start,
            fn = likelihood_function,
            data = clean_data,
            method = method,
            lower = c(-0.5, -0.5, 0.1),  # 下限を緩和
            upper = c(0.5, 0.5, 15.0),   # 上限を緩和
            control = list(
              maxit = 50000,     # より多くの反復回数
              reltol = 1e-12,    # より厳密な収束基準
              factr = 1e3,       # より緩やかな収束基準
              pgtol = 1e-10      # 勾配に対する収束基準
            ),
            hessian = TRUE
          )
        }
        
        if (result$convergence == 0) {
          # 標準誤差の計算
          if (!method == "SANN" && all(is.finite(result$hessian)) && det(result$hessian) > 1e-10) {
            se <- sqrt(diag(solve(result$hessian)))
            if (all(se < 1)) {  # 標準誤差が妥当な範囲内かチェック
              result$se <- se
              result$method <- method
              all_results[[length(all_results) + 1]] <- result
            }
          } else if (method == "SANN") {
            # SANNの場合は標準誤差を別途計算
            se <- calculate_bootstrap_se(clean_data, result$par)
            if (all(se < 1)) {
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

# 条件間の社会的選好パラメータの比較
compare_social_preferences <- function(results) {
  # 結果をデータフレームに整形
  comparison_df <- results %>%
    mutate(
      alpha_ci_lower = alpha - 1.96 * alpha_se,
      alpha_ci_upper = alpha + 1.96 * alpha_se,
      beta_ci_lower = beta - 1.96 * beta_se,
      beta_ci_upper = beta + 1.96 * beta_se,
      lambda_ci_lower = lambda - 1.96 * lambda_se,
      lambda_ci_upper = lambda + 1.96 * lambda_se
    )
  
  cat("\n=== 条件間の社会的選好パラメータ比較 ===\n\n")
  
  # 各条件のパラメータ推定値と95%信頼区間
  cat("1. 条件ごとのパラメータ推定値（95%信頼区間）:\n\n")
  for (cond in unique(comparison_df$condition)) {
    cat(sprintf("\n%s条件:\n", cond))
    cond_data <- comparison_df %>% filter(condition == cond)
    if (!all(is.na(cond_data$alpha))) {
      cat(sprintf("α (不利な不平等回避) = %.3f (%.3f, %.3f)\n", 
                 cond_data$alpha, cond_data$alpha_ci_lower, cond_data$alpha_ci_upper))
      cat(sprintf("β (有利な不平等回避) = %.3f (%.3f, %.3f)\n", 
                 cond_data$beta, cond_data$beta_ci_lower, cond_data$beta_ci_upper))
      cat(sprintf("λ (選択の感度) = %.3f (%.3f, %.3f)\n", 
                 cond_data$lambda, cond_data$lambda_ci_lower, cond_data$lambda_ci_upper))
    } else {
      cat("パラメータの推定に失敗しました\n")
    }
  }
  
  # 条件間の差の計算（AI - Control）
  if (nrow(comparison_df) == 2 && !any(is.na(comparison_df$alpha))) {
    ai_data <- comparison_df %>% filter(condition == "AI")
    control_data <- comparison_df %>% filter(condition == "Control")
    
    # 差の計算
    delta_alpha <- ai_data$alpha - control_data$alpha
    delta_beta <- ai_data$beta - control_data$beta
    delta_lambda <- ai_data$lambda - control_data$lambda
    
    # 差の標準誤差（Klockmann et al. 2022と同様）
    se_diff_alpha <- sqrt(ai_data$alpha_se^2 + control_data$alpha_se^2)
    se_diff_beta <- sqrt(ai_data$beta_se^2 + control_data$beta_se^2)
    se_diff_lambda <- sqrt(ai_data$lambda_se^2 + control_data$lambda_se^2)
    
    # z統計量とp値の計算
    z_alpha <- delta_alpha / se_diff_alpha
    z_beta <- delta_beta / se_diff_beta
    z_lambda <- delta_lambda / se_diff_lambda
    
    p_alpha <- 2 * (1 - pnorm(abs(z_alpha)))
    p_beta <- 2 * (1 - pnorm(abs(z_beta)))
    p_lambda <- 2 * (1 - pnorm(abs(z_lambda)))
    
    cat("\n2. 条件間の差（AI - Control）:\n")
    cat(sprintf("Δα = %.3f\n", delta_alpha))
    cat(sprintf("Δβ = %.3f\n", delta_beta))
    cat(sprintf("Δλ = %.3f\n", delta_lambda))
    
    cat("\n3. 統計的検定（z検定）:\n")
    cat(sprintf("α: z = %.3f, p = %.3f\n", z_alpha, p_alpha))
    cat(sprintf("β: z = %.3f, p = %.3f\n", z_beta, p_beta))
    cat(sprintf("λ: z = %.3f, p = %.3f\n", z_lambda, p_lambda))
    
    # 効果量の計算（Cohen's d）
    d_alpha <- delta_alpha / sqrt((ai_data$alpha_se^2 + control_data$alpha_se^2) / 2)
    d_beta <- delta_beta / sqrt((ai_data$beta_se^2 + control_data$beta_se^2) / 2)
    d_lambda <- delta_lambda / sqrt((ai_data$lambda_se^2 + control_data$lambda_se^2) / 2)
    
    cat("\n4. 効果量（Cohen's d）:\n")
    cat(sprintf("α: d = %.3f\n", d_alpha))
    cat(sprintf("β: d = %.3f\n", d_beta))
    cat(sprintf("λ: d = %.3f\n", d_lambda))
  }
  
  return(comparison_df)
}

# メイン実行部分
results <- estimate_by_condition(data_all)
comparison_results <- compare_social_preferences(results)

# 結果の表示
print("=== 分析結果 ===")
print(results)
