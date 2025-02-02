# AI2025実験分析

社会的選好と選択行動の分析結果

## リポジトリ構造

```
AI2025_R/
├── README.md                          # このファイル
├── .gitignore                         # Git除外設定
├── AI2025_choice_analysis.Rmd         # 選択行動の分析スクリプト
├── AI2025_socialPreference_analysis.R # 社会的選好の分析スクリプト
├── AI2025_payoffAvg.Rmd              # 利得分析スクリプト
└── AI2025_data/                      # 実験データ
    ├── 20250117_3/                   # AI条件（1回目）
    ├── 20250117_4/                   # コントロール条件（1回目）
    ├── 20250120_4/                   # コントロール条件（2回目）
    └── 20250120_5/                   # AI条件（2回目）
```

## 分析内容

1. **選択行動分析** (`AI2025_choice_analysis.Rmd`)
   - 条件別の選択率分析
   - 利害対立状況での選択行動
   - 不平等状況での分析
   - 時系列での選択率変化

2. **社会的選好分析** (`AI2025_socialPreference_analysis.R`)
   - 社会的選好パラメータの推定
   - 条件間の比較分析
   - 個人レベルの分析

3. **利得分析** (`AI2025_payoffAvg.Rmd`)
   - 平均利得の計算
   - 条件間の利得差の検定
   - 分配の公平性分析

## 使用方法

1. リポジトリのクローン:
```bash
git clone https://github.com/kitayama-arch/AI2025_R.git
```

2. 必要なRパッケージのインストール:
```r
install.packages(c("tidyverse", "kableExtra", "effectsize", "broom"))
```

3. 分析の実行:
```r
rmarkdown::render("AI2025_choice_analysis.Rmd")
```

## 注意事項

- データは非公開情報を含むため、適切な取り扱いが必要です
- 分析結果の公開前に、プライバシーに関する確認が必要です
