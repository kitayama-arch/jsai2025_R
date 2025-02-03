# AI2025実験 社会的選好パラメータ分析
# 作成日: Sys.Date()

# 1. 環境設定 ----
# 必要なパッケージの読み込み
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("maxLik")) install.packages("maxLik")
if (!require("boot")) install.packages("boot")
if (!require("gridExtra")) install.packages("gridExtra")

library(tidyverse)
library(maxLik)
library(boot)
library(gridExtra)

# 2. データの読み込みと前処理 ----
# AI条件のデータ
data_0117_3 <- read_csv("AI2025_data/20250117_3/dictator_app_2025-01-17.csv")
data_0120_5 <- read_csv("AI2025_data/20250120_5/dictator_app_2025-01-20.csv")
# コントロール条件のデータ
data_0117_4 <- read_csv("AI2025_data/20250117_4/Base_dictator_app_2025-01-17.csv")
data_0120_4 <- read_csv("AI2025_data/20250120_4/Base_dictator_app_2025-01-20.csv")

# データの確認
print("=== データの確認 ===")
print("AI条件（2025-01-17）のサンプル:")
print(head(data_0117_3 %>% select(player.choice, player.payoff_dictator, player.payoff_receiver)))
print("\n選択肢の分布:")
print(table(data_0117_3$player.choice))

# 前処理: 実際に参加した参加者のデータのみを抽出
data_0117_3_clean <- data_0117_3 %>%
  filter(participant.visited == 1) %>%
  mutate(choice_numeric = ifelse(player.choice == "Y", 1, 0))

data_0120_5_clean <- data_0120_5 %>%
  filter(participant.visited == 1) %>%
  mutate(choice_numeric = ifelse(player.choice == "Y", 1, 0))

# ベースライン条件ではラウンド16を除外
data_0117_4_clean <- data_0117_4 %>%
  filter(participant.visited == 1) %>%
  filter(subsession.round_number != 16) %>%
  mutate(choice_numeric = ifelse(player.choice == "Y", 1, 0))

data_0120_4_clean <- data_0120_4 %>%
  filter(participant.visited == 1) %>%
  filter(subsession.round_number != 16) %>%
  mutate(choice_numeric = ifelse(player.choice == "Y", 1, 0))

# データの統合
ai_processed <- bind_rows(
  data_0117_3_clean,
  data_0120_5_clean
) %>%
  mutate(
    s = as.numeric(player.payoff_receiver > player.payoff_dictator),
    r = as.numeric(player.payoff_dictator > player.payoff_receiver)
  )

control_processed <- bind_rows(
  data_0117_4_clean,
  data_0120_4_clean
) %>%
  mutate(
    s = as.numeric(player.payoff_receiver > player.payoff_dictator),
    r = as.numeric(player.payoff_dictator > player.payoff_receiver)
  )

# 処理後のデータ確認
print("\n=== 処理後のデータ確認 ===")
print("AI条件の処理後データ（先頭5行）:")
print(head(ai_processed %>% select(choice_numeric, player.payoff_dictator, player.payoff_receiver, s, r), 5))
print("\nAI条件の要約統計量:")
print(summary(ai_processed %>% select(choice_numeric, player.payoff_dictator, player.payoff_receiver, s, r)))

# 3. 効用関数の定義と最尤推定 ----
# テスト用の効用関数
test_utility <- function(data, alpha = 0.3, beta = 0.2) {
  utilities <- mapply(
    function(ps, po) {
      s <- as.numeric(po > ps)
      r <- as.numeric(ps > po)
      (1 - alpha * s - beta * r) * ps + (alpha * s + beta * r) * po
    },
    data$player.payoff_dictator,
    data$player.payoff_receiver
  )
  return(utilities)
}

# 効用関数のテスト
print("\n=== 効用関数のテスト ===")
test_results <- test_utility(head(ai_processed, 5))
print("最初の5行の効用値:")
print(test_results)

# 効用関数の定義
utility <- function(payoff_self, payoff_other, alpha, beta, lambda) {
  s <- as.numeric(payoff_other > payoff_self)
  r <- as.numeric(payoff_self > payoff_other)
  
  # 基本的な効用
  u <- (1 - alpha * s - beta * r) * payoff_self + (alpha * s + beta * r) * payoff_other
  
  # 選択の一貫性を考慮
  return(lambda * u)
}

# 対数尤度関数
log_likelihood <- function(params, data) {
  alpha <- params[1]
  beta <- params[2]
  lambda <- exp(params[3])  # 常に正の値を保証
  
  # パラメータの制約を緩和（ペナルティアプローチ）
  penalty <- 0
  if(abs(alpha) > 2) penalty <- penalty + (abs(alpha) - 2)^2
  if(abs(beta) > 2) penalty <- penalty + (abs(beta) - 2)^2
  if(lambda > 10) penalty <- penalty + (lambda - 10)^2
  
  # 効用の計算
  utilities <- mapply(
    function(ps, po) utility(ps, po, alpha, beta, lambda),
    data$player.payoff_dictator,
    data$player.payoff_receiver
  )
  
  # スケーリング（数値の安定性のため）
  utilities <- utilities / 100
  
  # ロジスティック選択確率
  probs <- 1 / (1 + exp(-utilities))
  
  # 数値の安定性のための調整
  probs[probs > 0.999999] <- 0.999999
  probs[probs < 0.000001] <- 0.000001
  
  # 対数尤度の計算
  ll <- sum(data$choice_numeric * log(probs) + (1 - data$choice_numeric) * log(1 - probs))
  
  # ペナルティの適用
  ll <- ll - 0.1 * penalty
  
  # NaNチェック
  if (is.nan(ll) || is.infinite(ll)) {
    warning("無効な対数尤度値が検出されました")
    return(-Inf)
  }
  
  return(ll)
}

# 対数尤度関数のテスト
print("\n=== 対数尤度関数のテスト ===")
test_ll <- log_likelihood(c(0.3, 0.2, log(1)), head(ai_processed, 5))
print("最初の5行での対数尤度値:")
print(test_ll)

# パラメータ推定関数
estimate_parameters <- function(data) {
  # 初期値のグリッド
  alphas <- seq(-0.5, 1.0, by = 0.25)
  betas <- seq(-0.5, 1.0, by = 0.25)
  lambdas <- seq(-1, 1, by = 0.5)  # log(lambda)の値
  
  start_values <- expand.grid(
    alpha = alphas,
    beta = betas,
    lambda = lambdas
  )
  
  # 最適化の設定
  control_settings <- list(
    tol = 1e-8,
    reltol = 1e-8,
    iterlim = 5000,
    printLevel = 2
  )
  
  # 各初期値での推定結果を保存
  results <- list()
  best_ll <- -Inf
  best_result <- NULL
  
  for(i in 1:nrow(start_values)) {
    tryCatch({
      result <- maxLik(
        logLik = log_likelihood,
        start = as.numeric(start_values[i,]),
        method = "BFGS",
        control = control_settings,
        data = data
      )
      
      if(result$maximum > best_ll && !any(is.na(sqrt(diag(vcov(result)))))) {
        best_ll <- result$maximum
        best_result <- result
      }
      
      results[[i]] <- result
    }, error = function(e) {
      warning(paste("初期値", i, "での推定エラー:", e$message))
    })
  }
  
  if(is.null(best_result)) {
    warning("すべての初期値で推定に失敗しました")
    return(NULL)
  }
  
  return(best_result)
}

# 4. パラメータ推定の実行 ----
# AI条件とコントロール条件それぞれで推定
ai_estimates <- estimate_parameters(ai_processed)
control_estimates <- estimate_parameters(control_processed)

if (is.null(ai_estimates) || is.null(control_estimates)) {
  stop("パラメータ推定に失敗しました")
}

# 結果の整理
format_results <- function(estimates, condition) {
  if (is.null(estimates)) return(NULL)
  data.frame(
    Condition = condition,
    Parameter = c("α", "β"),
    Estimate = coef(estimates),
    SE = sqrt(diag(vcov(estimates))),
    t_value = coef(estimates) / sqrt(diag(vcov(estimates)))
  )
}

ai_results <- format_results(ai_estimates, "AI")
control_results <- format_results(control_estimates, "Control")
results_table <- rbind(ai_results, control_results)

# 結果の表示
cat("\nパラメータ推定結果:\n")
print(results_table)

# 5. 仮説検定 ----
# パラメータ差のブートストラップ信頼区間
bootstrap_diff <- function(data1, data2, R = 1000) {
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  
  boot_results <- replicate(R, {
    # リサンプリング
    boot_data1 <- data1[sample(n1, replace = TRUE), ]
    boot_data2 <- data2[sample(n2, replace = TRUE), ]
    
    # パラメータ推定
    est1 <- estimate_parameters(boot_data1)
    est2 <- estimate_parameters(boot_data2)
    
    if (is.null(est1) || is.null(est2)) return(c(NA, NA))
    
    # 差の計算
    coef(est1) - coef(est2)
  })
  
  # NAを除外
  boot_results <- boot_results[, !apply(boot_results, 2, function(x) any(is.na(x)))]
  
  if (ncol(boot_results) < R * 0.5) {
    warning("ブートストラップの50%以上が失敗しました")
  }
  
  # 信頼区間の計算
  t(apply(boot_results, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE))
}

# 信頼区間の計算
ci_results <- bootstrap_diff(ai_processed, control_processed)
rownames(ci_results) <- c("α差", "β差")
colnames(ci_results) <- c("2.5%", "97.5%")

# 結果の表示
cat("\nブートストラップ信頼区間:\n")
print(ci_results)

# 6. 感度分析 ----
# 弾力性の計算
calculate_elasticity <- function(estimates, data) {
  if (is.null(estimates)) return(NULL)
  params <- coef(estimates)
  alpha <- params[1]
  beta <- params[2]
  
  # α に関する弾力性
  d_alpha <- with(data, {
    s <- as.numeric(player.payoff_receiver > player.payoff_dictator)
    mean(-s * player.payoff_dictator + s * player.payoff_receiver)
  })
  
  # β に関する弾力性
  d_beta <- with(data, {
    r <- as.numeric(player.payoff_dictator > player.payoff_receiver)
    mean(-r * player.payoff_dictator + r * player.payoff_receiver)
  })
  
  return(c(alpha_elasticity = d_alpha, beta_elasticity = d_beta))
}

# AI条件とコントロール条件の弾力性
ai_elasticity <- calculate_elasticity(ai_estimates, ai_processed)
control_elasticity <- calculate_elasticity(control_estimates, control_processed)

if (!is.null(ai_elasticity) && !is.null(control_elasticity)) {
  elasticity_table <- rbind(
    data.frame(Condition = "AI", t(ai_elasticity)),
    data.frame(Condition = "Control", t(control_elasticity))
  )
  
  # 結果の表示
  cat("\n弾力性分析結果:\n")
  print(elasticity_table)
}

# 7. 結果の解釈 ----
# パラメータパターンの判定
interpret_pattern <- function(ai_params, control_params) {
  if (is.null(ai_params) || is.null(control_params)) return("推定失敗")
  
  alpha_diff <- ai_params[1] - control_params[1]
  beta_diff <- ai_params[2] - control_params[2]
  
  pattern <- case_when(
    alpha_diff > 0 & beta_diff > 0 ~ "普遍的利他性",
    alpha_diff > 0 & abs(beta_diff) < 0.1 ~ "弱者保護指向",
    abs(alpha_diff) < 0.1 & beta_diff > 0 ~ "効率性重視",
    alpha_diff < 0 & beta_diff < 0 ~ "自己利益最大化",
    TRUE ~ "その他"
  )
  
  return(pattern)
}

pattern <- interpret_pattern(
  if (!is.null(ai_estimates)) coef(ai_estimates) else NULL,
  if (!is.null(control_estimates)) coef(control_estimates) else NULL
)

# 結果の表示
cat("\n解釈されたパターン:", pattern, "\n")

# 8. 診断チェック ----
# ヘッセ行列の正定値性チェック
check_hessian <- function(estimates) {
  if (is.null(estimates)) return(NA)
  tryCatch({
    eigen_values <- eigen(estimates$hessian)$values
    all(eigen_values < 0)  # 最大化問題なので負定値であることを確認
  }, error = function(e) {
    warning(paste("ヘッセ行列チェックエラー:", e$message))
    return(NA)
  })
}

# 診断結果の表示
ai_hessian_check <- check_hessian(ai_estimates)
control_hessian_check <- check_hessian(control_estimates)

diagnostic_results <- data.frame(
  Condition = c("AI", "Control"),
  Hessian_Check = c(ai_hessian_check, control_hessian_check)
)

cat("\n診断チェック結果:\n")
print(diagnostic_results)

# 9. 結果の保存 ----
# 結果をRDSファイルとして保存
results_list <- list(
  parameter_estimates = results_table,
  confidence_intervals = ci_results,
  elasticity = if (exists("elasticity_table")) elasticity_table else NULL,
  pattern = pattern,
  diagnostics = diagnostic_results
)

saveRDS(results_list, "social_preference_results.rds")

# CSVファイルとしても保存
write.csv(results_table, "parameter_estimates.csv", row.names = FALSE)
write.csv(ci_results, "confidence_intervals.csv")
if (exists("elasticity_table")) {
  write.csv(elasticity_table, "elasticity_results.csv", row.names = FALSE)
}
write.csv(diagnostic_results, "diagnostic_results.csv", row.names = FALSE)

# 結果の可視化と統計的検定
visualize_and_test_results <- function(ai_estimates, control_estimates) {
  # パラメータの信頼区間の計算
  get_ci <- function(estimates) {
    coef <- coef(estimates)
    se <- sqrt(diag(vcov(estimates)))
    data.frame(
      parameter = c("α", "β", "λ"),
      estimate = c(coef[1:2], exp(coef[3])),
      lower = c(coef[1:2], exp(coef[3])) - 1.96 * se,
      upper = c(coef[1:2], exp(coef[3])) + 1.96 * se
    )
  }
  
  ai_ci <- get_ci(ai_estimates)
  control_ci <- get_ci(control_estimates)
  
  # パラメータの比較プロット
  parameter_plot <- ggplot() +
    geom_pointrange(data = ai_ci,
                   aes(x = parameter, y = estimate, 
                       ymin = lower, ymax = upper, color = "AI")) +
    geom_pointrange(data = control_ci,
                   aes(x = parameter, y = estimate, 
                       ymin = lower, ymax = upper, color = "Control")) +
    labs(title = "社会的選好パラメータの推定結果",
         x = "パラメータ",
         y = "推定値",
         color = "条件") +
    theme_minimal() +
    theme(text = element_text(family = "HiraKakuProN-W3"))
  
  # プロットの保存
  ggsave("social_preference_parameters.png", parameter_plot, 
         width = 10, height = 6)
  
  # 統計的検定
  # パラメータの差のZ検定
  z_test <- function(est1, se1, est2, se2) {
    z_stat <- (est1 - est2) / sqrt(se1^2 + se2^2)
    p_value <- 2 * (1 - pnorm(abs(z_stat)))
    return(c(z_stat = z_stat, p_value = p_value))
  }
  
  # 各パラメータの差の検定
  ai_coef <- coef(ai_estimates)
  ai_se <- sqrt(diag(vcov(ai_estimates)))
  control_coef <- coef(control_estimates)
  control_se <- sqrt(diag(vcov(control_estimates)))
  
  test_results <- data.frame(
    parameter = c("α", "β", "λ"),
    z_statistic = numeric(3),
    p_value = numeric(3)
  )
  
  for(i in 1:3) {
    test <- z_test(ai_coef[i], ai_se[i], control_coef[i], control_se[i])
    test_results$z_statistic[i] <- test["z_stat"]
    test_results$p_value[i] <- test["p_value"]
  }
  
  # 結果の出力
  cat("\n=== 社会的選好パラメータの推定結果 ===\n")
  cat("\nAI条件:\n")
  print(ai_ci)
  cat("\nControl条件:\n")
  print(control_ci)
  cat("\n条件間の差の検定結果:\n")
  print(test_results)
  
  # 結果の保存
  results_list <- list(
    ai_estimates = ai_ci,
    control_estimates = control_ci,
    difference_tests = test_results,
    plot = parameter_plot
  )
  
  saveRDS(results_list, "social_preference_analysis_results.rds")
  write.csv(test_results, "parameter_difference_tests.csv", row.names = FALSE)
  
  return(results_list)
}

# 結果の分析実行
analysis_results <- visualize_and_test_results(ai_estimates, control_estimates)

# パターンの解釈
interpret_results <- function(analysis_results) {
  ai_params <- analysis_results$ai_estimates
  control_params <- analysis_results$control_estimates
  tests <- analysis_results$difference_tests
  
  cat("\n=== 社会的選好パターンの解釈 ===\n")
  
  # α（不利な不平等への感応度）の解釈
  cat("\n1. 不利な不平等への感応度 (α):\n")
  alpha_diff <- ai_params$estimate[1] - control_params$estimate[1]
  cat(sprintf("  - AI条件: %.3f (95%% CI: %.3f-%.3f)\n",
              ai_params$estimate[1], ai_params$lower[1], ai_params$upper[1]))
  cat(sprintf("  - Control条件: %.3f (95%% CI: %.3f-%.3f)\n",
              control_params$estimate[1], control_params$lower[1], control_params$upper[1]))
  cat(sprintf("  - 差: %.3f (p = %.3f)\n", alpha_diff, tests$p_value[1]))
  
  # β（有利な不平等への感応度）の解釈
  cat("\n2. 有利な不平等への感応度 (β):\n")
  beta_diff <- ai_params$estimate[2] - control_params$estimate[2]
  cat(sprintf("  - AI条件: %.3f (95%% CI: %.3f-%.3f)\n",
              ai_params$estimate[2], ai_params$lower[2], ai_params$upper[2]))
  cat(sprintf("  - Control条件: %.3f (95%% CI: %.3f-%.3f)\n",
              control_params$estimate[2], control_params$lower[2], control_params$upper[2]))
  cat(sprintf("  - 差: %.3f (p = %.3f)\n", beta_diff, tests$p_value[2]))
  
  # λ（選択の一貫性）の解釈
  cat("\n3. 選択の一貫性 (λ):\n")
  lambda_diff <- ai_params$estimate[3] - control_params$estimate[3]
  cat(sprintf("  - AI条件: %.3f (95%% CI: %.3f-%.3f)\n",
              ai_params$estimate[3], ai_params$lower[3], ai_params$upper[3]))
  cat(sprintf("  - Control条件: %.3f (95%% CI: %.3f-%.3f)\n",
              control_params$estimate[3], control_params$lower[3], control_params$upper[3]))
  cat(sprintf("  - 差: %.3f (p = %.3f)\n", lambda_diff, tests$p_value[3]))
  
  # 全体的なパターンの解釈
  cat("\n4. 全体的なパターン:\n")
  pattern <- case_when(
    alpha_diff > 0 & beta_diff > 0 ~ "普遍的利他性の増加",
    alpha_diff > 0 & abs(beta_diff) < 0.1 ~ "弱者保護指向の強化",
    abs(alpha_diff) < 0.1 & beta_diff > 0 ~ "効率性重視の傾向",
    alpha_diff < 0 & beta_diff < 0 ~ "自己利益最大化の傾向",
    TRUE ~ "混合パターン"
  )
  cat(sprintf("  - 観察されたパターン: %s\n", pattern))
  
  # 結果の保存
  interpretation <- list(
    pattern = pattern,
    alpha_difference = alpha_diff,
    beta_difference = beta_diff,
    lambda_difference = lambda_diff,
    p_values = tests$p_value
  )
  
  saveRDS(interpretation, "social_preference_interpretation.rds")
  
  return(interpretation)
}

# 結果の解釈実行
interpretation <- interpret_results(analysis_results)

# 結果の出力を改善
print_results <- function(ai_estimates, control_estimates) {
  cat("\n=== 社会的選好パラメータの推定結果 ===\n")
  
  # AI条件の結果
  cat("\nAI条件の推定結果:\n")
  ai_coef <- coef(ai_estimates)
  ai_se <- sqrt(diag(vcov(ai_estimates)))
  print(data.frame(
    Parameter = c("α", "β", "λ"),
    Estimate = ai_coef,
    Std_Error = ai_se
  ))
  
  # Control条件の結果
  cat("\nControl条件の推定結果:\n")
  control_coef <- coef(control_estimates)
  control_se <- sqrt(diag(vcov(control_estimates)))
  print(data.frame(
    Parameter = c("α", "β", "λ"),
    Estimate = control_coef,
    Std_Error = control_se
  ))
  
  # 結果をファイルに保存
  results_df <- rbind(
    data.frame(
      Condition = "AI",
      Parameter = c("α", "β", "λ"),
      Estimate = ai_coef,
      Std_Error = ai_se
    ),
    data.frame(
      Condition = "Control",
      Parameter = c("α", "β", "λ"),
      Estimate = control_coef,
      Std_Error = control_se
    )
  )
  
  # CSVファイルとして保存
  write.csv(results_df, "social_preference_parameters.csv", row.names = FALSE)
  
  # パラメータの差の検定
  z_stats <- (ai_coef - control_coef) / 
    sqrt(ai_se^2 + control_se^2)
  p_values <- 2 * (1 - pnorm(abs(z_stats)))
  
  difference_tests <- data.frame(
    Parameter = c("α", "β", "λ"),
    Difference = ai_coef - control_coef,
    Z_statistic = z_stats,
    P_value = p_values
  )
  
  cat("\n=== パラメータの差の検定結果 ===\n")
  print(difference_tests)
  
  # 検定結果をCSVファイルとして保存
  write.csv(difference_tests, "parameter_difference_tests.csv", row.names = FALSE)
  
  # 解釈
  cat("\n=== 結果の解釈 ===\n")
  
  # α（不利な不平等への感応度）の解釈
  cat("\n1. 不利な不平等への感応度 (α):\n")
  if (p_values[1] < 0.05) {
    cat(sprintf("  - 有意な差が検出されました (p = %.3f)\n", p_values[1]))
    if (ai_coef[1] > control_coef[1]) {
      cat("  - AI条件の方が不利な不平等に対してより敏感です\n")
    } else {
      cat("  - Control条件の方が不利な不平等に対してより敏感です\n")
    }
  } else {
    cat("  - 条件間で有意な差は検出されませんでした\n")
  }
  
  # β（有利な不平等への感応度）の解釈
  cat("\n2. 有利な不平等への感応度 (β):\n")
  if (p_values[2] < 0.05) {
    cat(sprintf("  - 有意な差が検出されました (p = %.3f)\n", p_values[2]))
    if (ai_coef[2] > control_coef[2]) {
      cat("  - AI条件の方が有利な不平等に対してより敏感です\n")
    } else {
      cat("  - Control条件の方が有利な不平等に対してより敏感です\n")
    }
  } else {
    cat("  - 条件間で有意な差は検出されませんでした\n")
  }
  
  # λ（選択の一貫性）の解釈
  cat("\n3. 選択の一貫性 (λ):\n")
  if (p_values[3] < 0.05) {
    cat(sprintf("  - 有意な差が検出されました (p = %.3f)\n", p_values[3]))
    if (ai_coef[3] > control_coef[3]) {
      cat("  - AI条件の方が選択がより一貫しています\n")
    } else {
      cat("  - Control条件の方が選択がより一貫しています\n")
    }
  } else {
    cat("  - 条件間で有意な差は検出されませんでした\n")
  }
  
  # 全体的なパターンの解釈
  cat("\n4. 全体的なパターン:\n")
  alpha_diff <- ai_coef[1] - control_coef[1]
  beta_diff <- ai_coef[2] - control_coef[2]
  
  pattern <- case_when(
    alpha_diff > 0 & beta_diff > 0 ~ "普遍的利他性の増加",
    alpha_diff > 0 & abs(beta_diff) < 0.1 ~ "弱者保護指向の強化",
    abs(alpha_diff) < 0.1 & beta_diff > 0 ~ "効率性重視の傾向",
    alpha_diff < 0 & beta_diff < 0 ~ "自己利益最大化の傾向",
    TRUE ~ "混合パターン"
  )
  
  cat(sprintf("  観察されたパターン: %s\n", pattern))
  
  # 結果をRDSファイルとして保存
  results_list <- list(
    parameters = results_df,
    difference_tests = difference_tests,
    pattern = pattern
  )
  
  saveRDS(results_list, "social_preference_results.rds")
  
  invisible(results_list)
}

# 結果の出力を実行
results <- print_results(ai_estimates, control_estimates) 