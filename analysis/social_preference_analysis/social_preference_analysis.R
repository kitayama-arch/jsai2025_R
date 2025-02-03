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
utility <- function(payoff_self, payoff_other, alpha, beta) {
  s <- as.numeric(payoff_other > payoff_self)
  r <- as.numeric(payoff_self > payoff_other)
  (1 - alpha * s - beta * r) * payoff_self + (alpha * s + beta * r) * payoff_other
}

# 対数尤度関数
log_likelihood <- function(params, data) {
  alpha <- params[1]
  beta <- params[2]
  
  # パラメータの制約チェック
  if(alpha < 0 || alpha > 1 || beta < 0 || beta > 1) return(-Inf)
  
  # 効用の計算
  utilities <- mapply(
    function(ps, po) utility(ps, po, alpha, beta),
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
  
  # NaNチェック
  if (is.nan(ll) || is.infinite(ll)) {
    warning("無効な対数尤度値が検出されました")
    return(-Inf)
  }
  
  return(ll)
}

# 対数尤度関数のテスト
print("\n=== 対数尤度関数のテスト ===")
test_ll <- log_likelihood(c(0.3, 0.2), head(ai_processed, 5))
print("最初の5行での対数尤度値:")
print(test_ll)

# パラメータ推定関数
estimate_parameters <- function(data) {
  # 初期値の設定
  start_values <- c(alpha = 0.3, beta = 0.2)
  
  # 最適化の制約
  lower <- c(0, 0)  # 下限
  upper <- c(1, 1)  # 上限
  
  tryCatch({
    result <- maxLik(
      logLik = log_likelihood,
      start = start_values,
      method = "BFGS",
      data = data,
      control = list(
        printLevel = 2,  # より詳細な出力
        tol = 1e-8,     # 収束判定の閾値
        reltol = 1e-8   # 相対的な収束判定の閾値
      ),
      constraints = list(
        ineqA = rbind(diag(2), -diag(2)),
        ineqB = c(upper, -lower)
      )
    )
    
    # 収束チェック
    if (!result$convergence) {
      warning("最適化が収束しませんでした")
      return(NULL)
    }
    
    # ヘッセ行列のチェック
    if (any(is.infinite(sqrt(diag(vcov(result)))))) {
      warning("無限大の標準誤差が検出されました")
      return(NULL)
    }
    
    return(result)
  }, error = function(e) {
    warning(paste("推定エラー:", e$message))
    return(NULL)
  })
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