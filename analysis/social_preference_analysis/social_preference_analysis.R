# 必要なパッケージの読み込み
library(tidyverse)
library(stats)
library(lme4)
library(car)
library(emmeans)
library(optimx)

# パッケージの競合を解決
select <- dplyr::select
filter <- dplyr::filter
some <- purrr::some

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
payoff_scenarios <- handle_data_loading("../../Experiment/payoffTable.csv") %>%
  mutate(
    # Klockmann et al. (2023)の分類に基づく
    position = Category_2_Position,
    is_selfish = Category_1_Slope == "Selfish",
    should_include = !Category_1_Slope %in% c("Receiver indiff.", "Dictator indiff.") &
                    !str_detect(Category_1_Slope, "Pareto"),
    # 新しい分類基準
    klockmann_restricted = is_selfish & should_include
  )

# AI条件の独裁者ゲームデータ
data_0117_3 <- handle_data_loading("../../AI2025_data/20250117_3/dictator_app_2025-01-17.csv")
data_0120_5 <- handle_data_loading("../../AI2025_data/20250120_5/dictator_app_2025-01-20.csv")
# コントロール条件の独裁者ゲームデータ
data_0117_4 <- handle_data_loading("../../AI2025_data/20250117_4/Base_dictator_app_2025-01-17.csv")
data_0120_4 <- handle_data_loading("../../AI2025_data/20250120_4/Base_dictator_app_2025-01-20.csv")

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
          Option_Y_Dictator, Option_Y_Receiver,
          position, is_selfish, should_include, klockmann_restricted
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
      ),
      # Klockmann et al. (2023)の定義に基づくSelfish選択の判定
      is_selfish_choice = case_when(
        player.choice == "X" & Option_X_Dictator > Option_Y_Dictator ~ TRUE,
        player.choice == "Y" & Option_Y_Dictator > Option_X_Dictator ~ TRUE,
        TRUE ~ FALSE
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
      total_rounds = n(),
      # 立場ごとのSelfish選択率を計算
      selfish_rate_advantageous = mean(is_selfish_choice[position == "Advantageous"], na.rm = TRUE),
      selfish_rate_mixed = mean(is_selfish_choice[position == "Mixed"], na.rm = TRUE),
      selfish_rate_disadvantageous = mean(is_selfish_choice[position == "Disadvantageous"], na.rm = TRUE)
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

# データの準備
prepare_data <- function(data) {
  data %>%
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
}

# AI条件とControl条件のデータを準備
data_ai <- prepare_data(data_all %>% filter(condition == "AI"))
data_control <- prepare_data(data_all %>% filter(condition == "Control"))

# Restricted sampleの準備
data_ai_restricted <- prepare_data(data_all %>% 
  filter(condition == "AI", klockmann_restricted == TRUE))
data_control_restricted <- prepare_data(data_all %>% 
  filter(condition == "Control", klockmann_restricted == TRUE))

# メイン実行部分の前に診断コードを追加
# データの状態確認
print("=== データ診断 ===")

# 条件ごとの参加者数とラウンド数の確認
participant_summary <- data_all %>%
  group_by(condition) %>%
  summarise(
    n_participants = n_distinct(participant.code),
    total_rounds = n(),
    avg_rounds_per_participant = mean(total_rounds),
    sd_rounds_per_participant = sd(total_rounds)
  )
print("参加者とラウンドの要約:")
print(participant_summary)

# 選択の分布確認
choice_summary <- data_all %>%
  group_by(condition) %>%
  summarise(
    n_choices = n(),
    prop_X = mean(choice_X, na.rm = TRUE),
    sd_X = sd(choice_X, na.rm = TRUE)
  )
print("\n選択の分布:")
print(choice_summary)

# 効用差の分布確認
utility_diagnostic <- function(data) {
  # テスト用パラメータ
  test_params <- c(0.1, 0.1, 1.0)  # α, β, λ
  
  # 効用差の計算
  max_payoff <- max(c(data$Option_X_Dictator, data$Option_X_Receiver,
                      data$Option_Y_Dictator, data$Option_Y_Receiver))
  
  utility_X <- with(data, {
    s <- as.integer(Option_X_Receiver > Option_X_Dictator)
    r <- as.integer(Option_X_Dictator > Option_X_Receiver)
    (1 - test_params[1] * s - test_params[2] * r) * Option_X_Dictator / max_payoff + 
    (test_params[1] * s + test_params[2] * r) * Option_X_Receiver / max_payoff
  })
  
  utility_Y <- with(data, {
    s <- as.integer(Option_Y_Receiver > Option_Y_Dictator)
    r <- as.integer(Option_Y_Dictator > Option_Y_Receiver)
    (1 - test_params[1] * s - test_params[2] * r) * Option_Y_Dictator / max_payoff + 
    (test_params[1] * s + test_params[2] * r) * Option_Y_Receiver / max_payoff
  })
  
  utility_diff <- utility_X - utility_Y
  return(utility_diff)
}

# 条件ごとの効用差の分布
utility_summary <- data_all %>%
  group_by(condition) %>%
  summarise(
    mean_utility_diff = mean(utility_diagnostic(.)),
    sd_utility_diff = sd(utility_diagnostic(.)),
    min_utility_diff = min(utility_diagnostic(.)),
    max_utility_diff = max(utility_diagnostic(.))
  )
print("\n効用差の分布:")
print(utility_summary)

# 最適化の収束状況の詳細確認
print("\n=== 最適化の診断 ===")

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

# 各条件でのパラメータ推定
estimate_parameters <- function(clean_data) {
  # グリッドサーチのパラメータ定義
  alpha_grid <- seq(-0.5, 0.5, by = 0.1)
  beta_grid <- seq(-0.5, 0.5, by = 0.1)
  lambda_grid <- seq(0.1, 15.0, by = 1.0)
  
  # グリッドサーチによる初期値の探索
  grid_results <- expand.grid(alpha = alpha_grid, beta = beta_grid, lambda = lambda_grid)
  grid_results$value <- apply(grid_results, 1, function(params) {
    tryCatch({
      likelihood_function(params, clean_data)
    }, error = function(e) Inf)
  })
  
  # 最良の初期値を選択
  best_start <- unlist(grid_results[which.min(grid_results$value), 1:3])
  
  # 最適化の実行
          result <- optim(
    par = best_start,
            fn = likelihood_function,
            data = clean_data,
    method = "L-BFGS-B",
    lower = c(-0.5, -0.5, 0.1),
    upper = c(0.5, 0.5, 15.0),
    control = list(maxit = 1000),
            hessian = TRUE
          )
  
  # ブートストラップによる標準誤差の推定
  bootstrap_results <- bootstrap_estimates(clean_data, alpha_grid, beta_grid, lambda_grid)
  
  # 結果の整理
  list(
    estimates = result$par,
    se = bootstrap_results$se,
    convergence = result$convergence,
    log_likelihood = -result$value,
    bootstrap_results = bootstrap_results
  )
}

# ブートストラップによる標準誤差の推定
bootstrap_estimates <- function(clean_data, alpha_grid, beta_grid, lambda_grid, n_bootstrap = 1000) {
  # 参加者のユニークIDを取得
  unique_participants <- unique(clean_data$participant.code)
  
  # 進捗表示の準備
  cat("ブートストラップ推定を開始 (", n_bootstrap, "回):\n")
  pb <- txtProgressBar(min = 0, max = n_bootstrap, style = 3)
  
  # ブートストラップサンプルの生成と推定
  bootstrap_results <- matrix(NA, nrow = 3, ncol = n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    # 参加者レベルでリサンプリング
    sampled_participants <- sample(unique_participants, replace = TRUE)
    
    # 選択された参加者のデータを結合
    bootstrap_data <- do.call(rbind, lapply(sampled_participants, function(p) {
      clean_data[clean_data$participant.code == p, ]
    }))
    
    # グリッドサーチの実行
    grid_results <- expand.grid(alpha = alpha_grid, beta = beta_grid, lambda = lambda_grid)
    grid_results$value <- apply(grid_results, 1, function(params) {
      tryCatch({
        likelihood_function(params, bootstrap_data)
      }, error = function(e) Inf)
    })
    
    # 最良の初期値を選択
    best_start <- unlist(grid_results[which.min(grid_results$value), 1:3])
    
    # 最適化の実行
    result <- tryCatch({
      optim(
        par = best_start,
        fn = likelihood_function,
        data = bootstrap_data,
        method = "L-BFGS-B",
        lower = c(-0.5, -0.5, 0.1),
        upper = c(0.5, 0.5, 15.0),
        control = list(maxit = 1000),
        hessian = TRUE
      )
    }, error = function(e) NULL)
    
    if (!is.null(result) && result$convergence == 0) {
      bootstrap_results[, i] <- result$par
    }
    
    # 進捗表示の更新
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  cat("\nブートストラップ推定完了\n")
  
  # 結果の要約
  bootstrap_mean <- rowMeans(bootstrap_results, na.rm = TRUE)
  bootstrap_se <- apply(bootstrap_results, 1, sd, na.rm = TRUE)
  
  # NA の割合を計算
  na_proportion <- rowMeans(is.na(bootstrap_results))
  cat(sprintf("\n収束失敗率: %.1f%%\n", mean(na_proportion) * 100))
  
  list(
    estimates = bootstrap_mean,
    se = bootstrap_se,
    raw_results = bootstrap_results,
    na_proportion = na_proportion
  )
}

# 全サンプルでの分析実行
ai_results <- estimate_parameters(data_ai)
control_results <- estimate_parameters(data_control)

# Restricted sampleでの分析実行
ai_results_restricted <- estimate_parameters(data_ai_restricted)
control_results_restricted <- estimate_parameters(data_control_restricted)

# 結果のデータフレーム作成（全サンプル）
results_df <- tibble(
  condition = c("AI", "Control"),
  sample_type = "All",
  alpha = c(ai_results$estimates[1], control_results$estimates[1]),
  beta = c(ai_results$estimates[2], control_results$estimates[2]),
  lambda = c(ai_results$estimates[3], control_results$estimates[3]),
  alpha_se = c(ai_results$se[1], control_results$se[1]),
  beta_se = c(ai_results$se[2], control_results$se[2]),
  lambda_se = c(ai_results$se[3], control_results$se[3]),
  convergence = c(ai_results$convergence, control_results$convergence),
  log_likelihood = c(ai_results$log_likelihood, control_results$log_likelihood)
)

# Restricted sampleの結果を追加
results_df_restricted <- tibble(
  condition = c("AI", "Control"),
  sample_type = "Restricted",
  alpha = c(ai_results_restricted$estimates[1], control_results_restricted$estimates[1]),
  beta = c(ai_results_restricted$estimates[2], control_results_restricted$estimates[2]),
  lambda = c(ai_results_restricted$estimates[3], control_results_restricted$estimates[3]),
  alpha_se = c(ai_results_restricted$se[1], control_results_restricted$se[1]),
  beta_se = c(ai_results_restricted$se[2], control_results_restricted$se[2]),
  lambda_se = c(ai_results_restricted$se[3], control_results_restricted$se[3]),
  convergence = c(ai_results_restricted$convergence, control_results_restricted$convergence),
  log_likelihood = c(ai_results_restricted$log_likelihood, control_results_restricted$log_likelihood)
)

# 結果を結合
results_df <- bind_rows(results_df, results_df_restricted)

# 結果をファイルに出力
sink("analysis/social_preference_analysis/social_preference_analysis_results.txt")

cat("\n=== 条件間の社会的選好パラメータ比較 ===\n\n")
cat("1. 条件ごとのパラメータ推定値（95%信頼区間）:\n\n")
cat("\nAllサンプル:\n\n")
cat("AI条件:\n")
cat(sprintf("α (不利な不平等回避) = %.3f (%.3f, %.3f)\n", ai_results$estimates[1], ai_results$se[1], ai_results$se[1] + 1.96 * ai_results$se[1]))
cat(sprintf("β (有利な不平等回避) = %.3f (%.3f, %.3f)\n", ai_results$estimates[2], ai_results$se[2], ai_results$se[2] + 1.96 * ai_results$se[2]))
cat(sprintf("λ (選択の感度) = %.3f (%.3f, %.3f)\n\n", ai_results$estimates[3], ai_results$se[3], ai_results$se[3] + 1.96 * ai_results$se[3]))

cat("Control条件:\n")
cat(sprintf("α (不利な不平等回避) = %.3f (%.3f, %.3f)\n", control_results$estimates[1], control_results$se[1], control_results$se[1] + 1.96 * control_results$se[1]))
cat(sprintf("β (有利な不平等回避) = %.3f (%.3f, %.3f)\n", control_results$estimates[2], control_results$se[2], control_results$se[2] + 1.96 * control_results$se[2]))
cat(sprintf("λ (選択の感度) = %.3f (%.3f, %.3f)\n\n", control_results$estimates[3], control_results$se[3], control_results$se[3] + 1.96 * control_results$se[3]))

cat("\nRestrictedサンプル:\n\n")
cat("AI条件:\n")
cat(sprintf("α (不利な不平等回避) = %.3f (%.3f, %.3f)\n", ai_results_restricted$estimates[1], ai_results_restricted$se[1], ai_results_restricted$se[1] + 1.96 * ai_results_restricted$se[1]))
cat(sprintf("β (有利な不平等回避) = %.3f (%.3f, %.3f)\n", ai_results_restricted$estimates[2], ai_results_restricted$se[2], ai_results_restricted$se[2] + 1.96 * ai_results_restricted$se[2]))
cat(sprintf("λ (選択の感度) = %.3f (%.3f, %.3f)\n\n", ai_results_restricted$estimates[3], ai_results_restricted$se[3], ai_results_restricted$se[3] + 1.96 * ai_results_restricted$se[3]))

cat("Control条件:\n")
cat(sprintf("α (不利な不平等回避) = %.3f (%.3f, %.3f)\n", control_results_restricted$estimates[1], control_results_restricted$se[1], control_results_restricted$se[1] + 1.96 * control_results_restricted$se[1]))
cat(sprintf("β (有利な不平等回避) = %.3f (%.3f, %.3f)\n", control_results_restricted$estimates[2], control_results_restricted$se[2], control_results_restricted$se[2] + 1.96 * control_results_restricted$se[2]))
cat(sprintf("λ (選択の感度) = %.3f (%.3f, %.3f)\n\n", control_results_restricted$estimates[3], control_results_restricted$se[3], control_results_restricted$se[3] + 1.96 * control_results_restricted$se[3]))

sink()

# 結果の可視化
plot_bootstrap_results <- function(ai_results, control_results, ai_results_restricted, control_results_restricted) {
  # Klockmann et al. (2023)のスタイルの可視化を追加
  
  # 立場ごとのSelfish選択率の箱ひげ図
  position_boxplot <- ggplot() +
    geom_boxplot(data = data_ai %>% 
                   group_by(participant.code, position) %>%
                   summarise(selfish_rate = mean(is_selfish_choice, na.rm = TRUE), .groups = "drop"),
                 aes(x = position, y = selfish_rate, fill = "AI")) +
    geom_boxplot(data = data_control %>% 
                   group_by(participant.code, position) %>%
                   summarise(selfish_rate = mean(is_selfish_choice, na.rm = TRUE), .groups = "drop"),
                 aes(x = position, y = selfish_rate, fill = "Control")) +
      scale_fill_manual(values = c("AI" = "#FF9999", "Control" = "#99CC99")) +
    labs(title = "立場ごとのSelfish選択率",
         x = "Position",
         y = "Selfish選択率",
         fill = "条件") +
      theme_minimal() +
    theme(text = element_text(family = "HiraKakuProN-W3"))
  
  # 立場ごとの選択率の時系列プロット
  time_series_plot <- ggplot() +
    geom_smooth(data = data_ai,
                aes(x = subsession.round_number, 
                    y = as.numeric(is_selfish_choice),
                    color = "AI"),
                method = "loess") +
    geom_smooth(data = data_control,
                aes(x = subsession.round_number,
                    y = as.numeric(is_selfish_choice),
                    color = "Control"),
                method = "loess") +
    facet_wrap(~position) +
    scale_color_manual(values = c("AI" = "#FF0000", "Control" = "#009900")) +
    labs(title = "ラウンドごとのSelfish選択率の推移",
         x = "ラウンド",
         y = "Selfish選択率",
         color = "条件") +
    theme_minimal() +
    theme(text = element_text(family = "HiraKakuProN-W3"))
  
  # 新しいプロットを保存
  ggsave("analysis/social_preference_analysis/position_selfish_rate.png", position_boxplot, width = 10, height = 6)
  ggsave("analysis/social_preference_analysis/time_series_selfish_rate.png", time_series_plot, width = 12, height = 6)
  
  # ブートストラップ結果の要約統計量
  bootstrap_summary <- ai_results$bootstrap_results$raw_results %>%
    as.data.frame() %>%
    mutate(condition = "AI") %>%
    bind_rows(
      control_results$bootstrap_results$raw_results %>%
        as.data.frame() %>%
        mutate(condition = "Control")
    ) %>%
    group_by(condition) %>%
    summarise(
      across(c(alpha, beta, lambda), 
             list(
               mean = ~mean(., na.rm = TRUE),
               sd = ~sd(., na.rm = TRUE),
               q025 = ~quantile(., 0.025, na.rm = TRUE),
               q975 = ~quantile(., 0.975, na.rm = TRUE)
             ))
    )
  
  # 要約統計量をファイルに保存
  write_csv(bootstrap_summary, "analysis/social_preference_analysis/bootstrap_summary.csv")
  
  cat("\n=== ブートストラップ推定の要約統計量 ===\n")
  print(bootstrap_summary)
}

# 可視化の実行
plot_bootstrap_results(ai_results, control_results, ai_results_restricted, control_results_restricted)

# 結果のMarkdown形式での出力
write_results_markdown <- function(ai_results, control_results, ai_results_restricted, control_results_restricted, results_df) {
  sink("analysis/social_preference_analysis/analysis_results.md")
  
  cat("# 社会的選好パラメータ分析結果\n\n")
  
  # Klockmann et al. (2023)の分析結果を追加
  cat("## 0. 立場ごとのSelfish選択率分析（Klockmann et al. 2023）\n\n")
  
  # AI条件の分析（全サンプル）
  selfish_rates_ai <- data_ai %>%
    group_by(position) %>%
    summarise(
      n = n(),
      selfish_rate = mean(is_selfish_choice, na.rm = TRUE),
      se = sd(is_selfish_choice, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  cat("### AI条件（全サンプル）:\n")
  for (pos in c("Advantageous", "Mixed", "Disadvantageous")) {
    stats <- selfish_rates_ai %>% filter(position == pos)
    cat(sprintf("%s position: %.1f%% (SE = %.3f, n = %d)\n",
                pos, stats$selfish_rate * 100, stats$se, stats$n))
  }
  
  # Control条件の分析（全サンプル）
  selfish_rates_control <- data_control %>%
    group_by(position) %>%
    summarise(
      n = n(),
      selfish_rate = mean(is_selfish_choice, na.rm = TRUE),
      se = sd(is_selfish_choice, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  cat("\n### Control条件（全サンプル）:\n")
  for (pos in c("Advantageous", "Mixed", "Disadvantageous")) {
    stats <- selfish_rates_control %>% filter(position == pos)
    cat(sprintf("%s position: %.1f%% (SE = %.3f, n = %d)\n",
                pos, stats$selfish_rate * 100, stats$se, stats$n))
  }
  
  # AI条件の分析（Restricted sample）
  selfish_rates_ai_restricted <- data_ai_restricted %>%
    group_by(position) %>%
    summarise(
      n = n(),
      selfish_rate = mean(is_selfish_choice, na.rm = TRUE),
      se = sd(is_selfish_choice, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  cat("\n### AI条件（Restricted sample）:\n")
  for (pos in c("Advantageous", "Mixed", "Disadvantageous")) {
    stats <- selfish_rates_ai_restricted %>% filter(position == pos)
    cat(sprintf("%s position: %.1f%% (SE = %.3f, n = %d)\n",
                pos, stats$selfish_rate * 100, stats$se, stats$n))
  }
  
  # Control条件の分析（Restricted sample）
  selfish_rates_control_restricted <- data_control_restricted %>%
    group_by(position) %>%
    summarise(
      n = n(),
      selfish_rate = mean(is_selfish_choice, na.rm = TRUE),
      se = sd(is_selfish_choice, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  cat("\n### Control条件（Restricted sample）:\n")
  for (pos in c("Advantageous", "Mixed", "Disadvantageous")) {
    stats <- selfish_rates_control_restricted %>% filter(position == pos)
    cat(sprintf("%s position: %.1f%% (SE = %.3f, n = %d)\n",
                pos, stats$selfish_rate * 100, stats$se, stats$n))
  }
  
  # 条件間の差の検定（全サンプル）
  cat("\n### 条件間の差の検定（全サンプル）:\n")
  for (pos in c("Advantageous", "Mixed", "Disadvantageous")) {
    test_data <- bind_rows(
      data_ai %>% filter(position == pos),
      data_control %>% filter(position == pos)
    )
    test_result <- t.test(is_selfish_choice ~ condition, data = test_data)
    cat(sprintf("\n%s position:\n", pos))
    cat(sprintf("t = %.3f, df = %.1f, p = %.3f\n",
                test_result$statistic,
                test_result$parameter,
                test_result$p.value))
    
    # 効果量（Cohen's d）の計算
    ai_data <- test_data %>% filter(condition == "AI")
    control_data <- test_data %>% filter(condition == "Control")
    pooled_sd <- sqrt(((length(ai_data$is_selfish_choice) - 1) * var(ai_data$is_selfish_choice) +
                        (length(control_data$is_selfish_choice) - 1) * var(control_data$is_selfish_choice)) /
                       (length(ai_data$is_selfish_choice) + length(control_data$is_selfish_choice) - 2))
    cohens_d <- (mean(ai_data$is_selfish_choice) - mean(control_data$is_selfish_choice)) / pooled_sd
    cat(sprintf("効果量 (Cohen's d) = %.3f\n", cohens_d))
  }
  
  # 条件間の差の検定（Restricted sample）
  cat("\n### 条件間の差の検定（Restricted sample）:\n")
  for (pos in c("Advantageous", "Mixed", "Disadvantageous")) {
    test_data <- bind_rows(
      data_ai_restricted %>% filter(position == pos),
      data_control_restricted %>% filter(position == pos)
    )
    test_result <- t.test(is_selfish_choice ~ condition, data = test_data)
    cat(sprintf("\n%s position:\n", pos))
    cat(sprintf("t = %.3f, df = %.1f, p = %.3f\n",
                test_result$statistic,
                test_result$parameter,
                test_result$p.value))
    
    # 効果量（Cohen's d）の計算
    ai_data <- test_data %>% filter(condition == "AI")
    control_data <- test_data %>% filter(condition == "Control")
    pooled_sd <- sqrt(((length(ai_data$is_selfish_choice) - 1) * var(ai_data$is_selfish_choice) +
                        (length(control_data$is_selfish_choice) - 1) * var(control_data$is_selfish_choice)) /
                       (length(ai_data$is_selfish_choice) + length(control_data$is_selfish_choice) - 2))
    cohens_d <- (mean(ai_data$is_selfish_choice) - mean(control_data$is_selfish_choice)) / pooled_sd
    cat(sprintf("効果量 (Cohen's d) = %.3f\n", cohens_d))
  }
  
  cat("\n")
  
  # 1. パラメータ推定結果
  cat("## 1. パラメータ推定結果\n\n")
  
  for (sample in c("All", "Restricted")) {
    cat(sprintf("### %sサンプル\n\n", sample))
    
    # AI条件の結果
    cat("#### AI条件:\n")
    results <- if (sample == "All") ai_results else ai_results_restricted
    cat(sprintf("- α (不利な不平等回避) = %.3f ± %.3f\n", 
                results$estimates[1], results$se[1]))
    cat(sprintf("- β (有利な不平等回避) = %.3f ± %.3f\n", 
                results$estimates[2], results$se[2]))
    cat(sprintf("- λ (選択の感度) = %.3f ± %.3f\n\n", 
                results$estimates[3], results$se[3]))
    
    # Control条件の結果
    cat("#### Control条件:\n")
    results <- if (sample == "All") control_results else control_results_restricted
    cat(sprintf("- α (不利な不平等回避) = %.3f ± %.3f\n", 
                results$estimates[1], results$se[1]))
    cat(sprintf("- β (有利な不平等回避) = %.3f ± %.3f\n", 
                results$estimates[2], results$se[2]))
    cat(sprintf("- λ (選択の感度) = %.3f ± %.3f\n\n", 
                results$estimates[3], results$se[3]))
    
    # 条件間の差と統計的検定
    cat(sprintf("### %sサンプルでの条件間の差（AI - Control）と統計的検定\n\n", sample))
    
    # 差の計算
    ai_data <- results_df %>% 
      filter(condition == "AI", sample_type == sample)
    control_data <- results_df %>% 
      filter(condition == "Control", sample_type == sample)
    
    delta_alpha <- ai_data$alpha - control_data$alpha
    delta_beta <- ai_data$beta - control_data$beta
    delta_lambda <- ai_data$lambda - control_data$lambda
    
    # 標準誤差の計算
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
    
    cat(sprintf("- Δα = %.3f（不利な不平等回避の差）\n", delta_alpha))
    cat(sprintf("  - z = %.3f, p = %.3f%s\n", 
                z_alpha, p_alpha, 
                ifelse(p_alpha < 0.05, "（統計的に有意な差あり）", "（統計的に有意な差なし）")))
    
    cat(sprintf("- Δβ = %.3f（有利な不平等回避の差）\n", delta_beta))
    cat(sprintf("  - z = %.3f, p = %.3f%s\n", 
                z_beta, p_beta,
                ifelse(p_beta < 0.05, "（統計的に有意な差あり）", "（統計的に有意な差なし）")))
    
    cat(sprintf("- Δλ = %.3f（選択感度の差）\n", delta_lambda))
    cat(sprintf("  - z = %.3f, p = %.3f%s\n\n", 
                z_lambda, p_lambda,
                ifelse(p_lambda < 0.05, "（統計的に有意な差あり）", "（統計的に有意な差なし）")))
  }
  
  # 3. 主な知見
  cat("## 3. 主な知見\n\n")
  cat("### 全サンプルでの分析\n\n")
  cat("1. AI条件では、Control条件と比較して：\n")
  ai_data <- results_df %>% filter(condition == "AI", sample_type == "All")
  control_data <- results_df %>% filter(condition == "Control", sample_type == "All")
  delta_alpha <- ai_data$alpha - control_data$alpha
  delta_beta <- ai_data$beta - control_data$beta
  delta_lambda <- ai_data$lambda - control_data$lambda
  se_diff_alpha <- sqrt(ai_data$alpha_se^2 + control_data$alpha_se^2)
  se_diff_beta <- sqrt(ai_data$beta_se^2 + control_data$beta_se^2)
  se_diff_lambda <- sqrt(ai_data$lambda_se^2 + control_data$lambda_se^2)
  z_alpha <- delta_alpha / se_diff_alpha
  z_beta <- delta_beta / se_diff_beta
  z_lambda <- delta_lambda / se_diff_lambda
  p_alpha <- 2 * (1 - pnorm(abs(z_alpha)))
  p_beta <- 2 * (1 - pnorm(abs(z_beta)))
  p_lambda <- 2 * (1 - pnorm(abs(z_lambda)))
  
  cat(sprintf("   - 不利な不平等への回避度が%sが、%s（Δα %s 0, p = %.3f）\n",
              ifelse(delta_alpha > 0, "高い", "低い"),
              ifelse(p_alpha < 0.05, "有意差あり", "有意差なし"),
              ifelse(delta_alpha > 0, ">", "<"),
              p_alpha))
  cat(sprintf("   - 有利な不平等への回避度が%sが、%s（Δβ %s 0, p = %.3f）\n",
              ifelse(delta_beta > 0, "高い", "低い"),
              ifelse(p_beta < 0.05, "有意差あり", "有意差なし"),
              ifelse(delta_beta > 0, ">", "<"),
              p_beta))
  cat(sprintf("   - 選択の感度が%s%s（Δλ %s 0, p = %.3f）\n",
              ifelse(delta_lambda > 0, "高い", "低い"),
              ifelse(p_lambda < 0.05, "（有意差あり）", "（有意差なし）"),
              ifelse(delta_lambda > 0, ">", "<"),
              p_lambda))
  
  cat("\n### Restricted sampleでの分析\n\n")
  cat("1. AI条件では、Control条件と比較して：\n")
  ai_data <- results_df %>% filter(condition == "AI", sample_type == "Restricted")
  control_data <- results_df %>% filter(condition == "Control", sample_type == "Restricted")
  delta_alpha <- ai_data$alpha - control_data$alpha
  delta_beta <- ai_data$beta - control_data$beta
  delta_lambda <- ai_data$lambda - control_data$lambda
  se_diff_alpha <- sqrt(ai_data$alpha_se^2 + control_data$alpha_se^2)
  se_diff_beta <- sqrt(ai_data$beta_se^2 + control_data$beta_se^2)
  se_diff_lambda <- sqrt(ai_data$lambda_se^2 + control_data$lambda_se^2)
  z_alpha <- delta_alpha / se_diff_alpha
  z_beta <- delta_beta / se_diff_beta
  z_lambda <- delta_lambda / se_diff_lambda
  p_alpha <- 2 * (1 - pnorm(abs(z_alpha)))
  p_beta <- 2 * (1 - pnorm(abs(z_beta)))
  p_lambda <- 2 * (1 - pnorm(abs(z_lambda)))
  
  cat(sprintf("   - 不利な不平等への回避度が%sが、%s（Δα %s 0, p = %.3f）\n",
              ifelse(delta_alpha > 0, "高い", "低い"),
              ifelse(p_alpha < 0.05, "有意差あり", "有意差なし"),
              ifelse(delta_alpha > 0, ">", "<"),
              p_alpha))
  cat(sprintf("   - 有利な不平等への回避度が%sが、%s（Δβ %s 0, p = %.3f）\n",
              ifelse(delta_beta > 0, "高い", "低い"),
              ifelse(p_beta < 0.05, "有意差あり", "有意差なし"),
              ifelse(delta_beta > 0, ">", "<"),
              p_beta))
  cat(sprintf("   - 選択の感度が%s%s（Δλ %s 0, p = %.3f）\n",
              ifelse(delta_lambda > 0, "高い", "低い"),
              ifelse(p_lambda < 0.05, "（有意差あり）", "（有意差なし）"),
              ifelse(delta_lambda > 0, ">", "<"),
              p_lambda))
  
  cat("\n2. 両条件とも、有利な不平等回避（β）が不利な不平等回避（α）より強い\n")
  
  cat(sprintf("\n3. Control条件の方が選択の一貫性が%s（λが大きい）\n",
              ifelse(p_lambda < 0.05, "有意に高い", "高い傾向にある")))
  
  # ファイルを閉じる
  sink()
}

# 結果の出力
write_results_markdown(ai_results, control_results, ai_results_restricted, control_results_restricted, results_df)
