# AI2025実験：選択行動分析
# AI学会実験チーム

# 必要なパッケージのインストール（必要な場合のみ実行）
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("kableExtra")) install.packages("kableExtra")
if (!require("effectsize")) install.packages("effectsize")
if (!require("broom")) install.packages("broom")
if (!require("lme4")) install.packages("lme4")
if (!require("ggplot2")) install.packages("ggplot2")

# パッケージの読み込み
library(tidyverse)
library(kableExtra)
library(effectsize)
library(broom)
library(lme4)
library(ggplot2)

# データの読み込みと前処理
# AI条件のデータ読み込み
ai_data_1 <- read_csv("AI2025_data/20250117_3/dictator_app_2025-01-17.csv", show_col_types = FALSE)
ai_data_2 <- read_csv("AI2025_data/20250120_5/dictator_app_2025-01-20.csv", show_col_types = FALSE)

# コントロール条件のデータ読み込み
control_data_1 <- read_csv("AI2025_data/20250117_4/Base_dictator_app_2025-01-17.csv", show_col_types = FALSE)
control_data_2 <- read_csv("AI2025_data/20250120_4/Base_dictator_app_2025-01-20.csv", show_col_types = FALSE)

# データの結合と前処理
df <- bind_rows(
  mutate(ai_data_1, condition = "AI"),
  mutate(ai_data_2, condition = "AI"),
  mutate(control_data_1, condition = "Control"),
  mutate(control_data_2, condition = "Control")
) %>%
  filter(participant.visited == 1) %>%
  filter(!(condition == "Control" & subsession.round_number == 16)) %>%
  mutate(choice_x = as.numeric(player.choice == "X"))  # 選択を数値型に変換

print("【データセットの基本情報】")
cat(sprintf("総サンプルサイズ: %d\n", nrow(df)))
cat(sprintf("AI条件のサンプル数: %d\n", sum(df$condition == "AI")))
cat(sprintf("Control条件のサンプル数: %d\n", sum(df$condition == "Control")))

# 条件別の選択率計算
choice_rates <- df %>%
  group_by(condition) %>%
  summarise(
    n_total = n(),
    n_x = sum(choice_x),
    x_rate = mean(choice_x),
    se = sqrt(x_rate * (1 - x_rate) / n_total)
  )

print("\n【条件別の選択率】")
print(choice_rates)

# 条件間の選択率の差の検定
choice_test <- t.test(
  choice_x ~ condition,
  data = df,
  var.equal = FALSE
)

print("\n【t検定の結果】")
print(choice_test)

# 効果量の計算
effect_size <- cohens_d(
  choice_x ~ condition,
  data = df
)

print("\n【効果量】")
print(effect_size)

# 利害対立状況の分析
df_conflict <- df %>%
  group_by(participant.code) %>%
  mutate(
    payoff_diff = player.payoff_dictator - player.payoff_receiver,
    is_conflict = payoff_diff > 0
  )

print("\n【利害対立状況の基本統計量】")
df_conflict %>%
  group_by(condition, is_conflict) %>%
  summarise(
    n = n(),
    mean_payoff_diff = mean(payoff_diff),
    sd_payoff_diff = sd(payoff_diff),
    .groups = "drop"
  ) %>%
  print()

# 利害対立状況での選択率分析
conflict_rates <- df_conflict %>%
  filter(is_conflict) %>%
  group_by(condition) %>%
  summarise(
    n_total = n(),
    n_x = sum(choice_x),
    x_rate = mean(choice_x),
    se = sqrt(x_rate * (1 - x_rate) / n_total)
  )

print("\n【利害対立状況での選択率】")
print(conflict_rates)

# 利害対立状況での条件間差の検定
conflict_test <- df_conflict %>%
  filter(is_conflict) %>%
  {t.test(
    choice_x ~ condition,
    data = .,
    var.equal = FALSE
  )}

print("\n【利害対立状況でのt検定結果】")
print(conflict_test)

# 利害対立状況での効果量
conflict_effect <- df_conflict %>%
  filter(is_conflict) %>%
  {cohens_d(
    choice_x ~ condition,
    data = .
  )}

print("\n【利害対立状況での効果量】")
print(conflict_effect)

# 利他性分析の追加
cat("\n## 利他性分析\n")

# 利他的選択の定義を追加
df <- df %>%
  mutate(
    is_altruistic = player.payoff_receiver > player.payoff_dictator,
    sacrifice = ifelse(is_altruistic, player.payoff_receiver - player.payoff_dictator, 0),
    altruism_score = scales::rescale(sacrifice, to = c(0, 1))
  )

# 条件別の利他的選択分析
altruism_rates <- df %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    altruistic_rate = mean(is_altruistic),
    mean_sacrifice = mean(sacrifice),
    median_altruism = median(altruism_score),
    .groups = "drop"
  )

print("【条件別利他性指標】")
print(altruism_rates)

# 利他的選択の条件間差の検定
altruism_test <- t.test(
  is_altruistic ~ condition,
  data = df,
  var.equal = FALSE
)

print("\n【利他的選択のt検定結果】")
print(altruism_test)

# 効果量計算
altruism_d <- cohens_d(
  as.numeric(is_altruistic) ~ condition,
  data = df,
  pooled_sd = TRUE
)

print("\n【利他的選択の効果量】")
print(altruism_d)

# 混合効果モデル
altruism_model <- glmer(
  is_altruistic ~ condition + (1 | participant.code),
  data = df,
  family = binomial()
)

print("\n【混合効果モデル結果】")
print(summary(altruism_model))

# 可視化
# 利他性選択率の比較プロット
p1 <- ggplot(altruism_rates, aes(condition, altruistic_rate, fill = condition)) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = altruistic_rate - 1.96*sqrt(altruistic_rate*(1-altruistic_rate)/n),
      ymax = altruistic_rate + 1.96*sqrt(altruistic_rate*(1-altruistic_rate)/n)
    ),
    width = 0.2
  ) +
  labs(
    title = "条件別利他的選択率",
    x = "実験条件",
    y = "利他的選択率"
  ) +
  theme_minimal()

print(p1)

# 自己犠牲度の分布プロット
p2 <- ggplot(df %>% filter(is_altruistic), aes(sacrifice, fill = condition)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "自己犠牲度の分布",
    x = "受け手への追加利得",
    y = "密度"
  ) +
  theme_minimal()

print(p2)

# 時系列分析（ラウンド別の利他性）
cat("\n## ラウンド別分析\n")

# ラウンドごとの利他的選択率
round_altruism <- df %>%
  group_by(condition, subsession.round_number) %>%
  summarise(
    n = n(),
    altruistic_rate = mean(is_altruistic),
    mean_sacrifice = mean(sacrifice),
    .groups = "drop"
  )

print("【ラウンド別利他的選択率】")
print(round_altruism)

# 可視化：ラウンドごとの利他的選択率の推移
p3 <- ggplot(round_altruism, 
       aes(x = subsession.round_number, 
           y = altruistic_rate, 
           color = condition, 
           group = condition)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "ラウンド別の利他的選択率の推移",
    x = "ラウンド",
    y = "利他的選択率",
    color = "条件"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, max(df$subsession.round_number), by = 1))

print(p3)

# 時系列効果の検定（混合効果モデル）
library(lme4)
time_model <- glmer(
  is_altruistic ~ condition * subsession.round_number + (1 | participant.code),
  data = df,
  family = binomial()
)

print("\n【時系列効果の分析結果（混合効果モデル）】")
print(summary(time_model))

# 前半と後半での比較
df <- df %>%
  mutate(
    phase = ifelse(subsession.round_number <= max(subsession.round_number)/2, 
                  "前半", "後半")
  )

phase_comparison <- df %>%
  group_by(condition, phase) %>%
  summarise(
    n = n(),
    altruistic_rate = mean(is_altruistic),
    mean_sacrifice = mean(sacrifice),
    .groups = "drop"
  )

print("\n【実験前半/後半の比較】")
print(phase_comparison)

# 前半と後半での利他的選択率の比較
cat("\n## 前半・後半の比較分析\n")
df <- df %>%
  group_by(participant.code) %>%
  mutate(
    session_half = ifelse(subsession.round_number <= max(subsession.round_number)/2, "前半", "後半")
  ) %>%
  ungroup()

half_comparison <- df %>%
  group_by(condition, session_half) %>%
  summarise(
    n = n(),
    altruistic_rate = mean(is_altruistic),
    se = sd(is_altruistic)/sqrt(n()),
    .groups = "drop"
  )

print("【前半・後半の利他的選択率】")
print(half_comparison)

# 個人レベルの一貫性分析
individual_consistency <- df %>%
  group_by(condition, participant.code) %>%
  summarise(
    n_rounds = n(),
    altruistic_rate = mean(is_altruistic),
    consistency = sd(is_altruistic),  # 低いほど一貫している
    .groups = "drop"
  )

print("\n【個人レベルの利他性分析】")
print(summary(individual_consistency))

# 混合効果モデル：ラウンドと条件の交互作用
library(lme4)
altruism_mixed <- glmer(
  is_altruistic ~ condition * scale(subsession.round_number) + 
    (1 | participant.code),
  family = binomial,
  data = df
)

print("\n【混合効果モデルの結果】")
print(summary(altruism_mixed))

# 可視化：個人レベルの利他的選択率の分布
p4 <- ggplot(individual_consistency, aes(x = altruistic_rate, fill = condition)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "個人レベルの利他的選択率の分布",
    x = "利他的選択率",
    y = "密度",
    fill = "条件"
  ) +
  theme_minimal()

print(p4)

# 結果の保存用文字列作成
result_text <- sprintf("
## 5. 利他性分析（追加分析）
### 利他的選択率
- AI条件: %.3f
- Control条件: %.3f

### 統計的検定結果
- t値: %.3f
- p値: %.3f
- 95%%信頼区間: [%.3f, %.3f]

### 効果量
- Cohen's d: %.3f
- 95%%信頼区間: [%.3f, %.3f]
",
  altruism_rates$altruistic_rate[altruism_rates$condition == "AI"],
  altruism_rates$altruistic_rate[altruism_rates$condition == "Control"],
  altruism_test$statistic,
  altruism_test$p.value,
  altruism_test$conf.int[1],
  altruism_test$conf.int[2],
  altruism_d$Cohens_d,
  altruism_d$CI_low,
  altruism_d$CI_high
)

cat(result_text)

# 結果の文字列作成
result_text_time <- sprintf("
### 時系列分析
1. **ラウンド効果**
   - 条件×ラウンドの交互作用: z = %.3f, p = %.3f
   - ラウンドの主効果: z = %.3f, p = %.3f

2. **前半/後半の比較**
   - AI条件
     - 前半: %.1f%%
     - 後半: %.1f%%
   - Control条件
     - 前半: %.1f%%
     - 後半: %.1f%%
",
  fixef(time_model)[4]/sqrt(vcov(time_model)[4,4]),
  2*(1-pnorm(abs(fixef(time_model)[4]/sqrt(vcov(time_model)[4,4])))),
  fixef(time_model)[3]/sqrt(vcov(time_model)[3,3]),
  2*(1-pnorm(abs(fixef(time_model)[3]/sqrt(vcov(time_model)[3,3])))),
  filter(phase_comparison, condition == "AI" & phase == "前半")$altruistic_rate * 100,
  filter(phase_comparison, condition == "AI" & phase == "後半")$altruistic_rate * 100,
  filter(phase_comparison, condition == "Control" & phase == "前半")$altruistic_rate * 100,
  filter(phase_comparison, condition == "Control" & phase == "後半")$altruistic_rate * 100
)

cat(result_text_time)
