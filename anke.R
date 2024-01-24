# アンケート分析
library(tidyverse)
library(psych)

# データのURL
url <- "https://so-ichi.com/anke.csv"
# データを読み込む
df <- read_csv(url)


# アンケートデータだけ抽出
df_q <- df |>
  select(2:24) |>
  select(-"ちょっと疲れてきたところだと思いますが，もう少しです。4を選択してください。")
names(df_q) <- paste0("Q", 1:22)

# アンケート項目の平均値で棒グラフ
df_q |>
  summarise(
    across(everything(), mean) # 全項目の平均
  ) |> 
  pivot_longer(everything(), names_to = "Q", values_to = "mean") |>
  ggplot() + aes(x = reorder(Q, mean), y = mean) + geom_col() +
  coord_flip() + theme_few() + xlab("平均値") + ylab("質問項目") +
  theme(text = element_text(size = 24))


# 因子数の決定
eigenvalues <- df_q |>
  cor() |> # 相関係数行列
  round(3) |> # 小数点以下3ケタで四捨五入
  eigen() %>% .$values # 固有値の値

# 固有値と因子数のグラフ
plot(eigenvalues, type = "b") # 折れ線グラフ
abline(h = 1, col = "red", lty = 2) # 横軸の1の線を引く

correlation <- df_q |> cor()

# 因子数3で因子分析
result <- fa( # 因子分析
  correlation, # 相関係数行列
  nfactors = 3, # 因子数3
  rotate = "varimax", # バリマックス回転
  fm = "regression" # 最小二乗法
  )

# 因子負荷量
print(result$loadings,
      digits = 2,   # 小数点以下2ケタで四捨五入
      cutoff = 0.5, # 表示する因子負荷量の閾値
      sort = TRUE)  # 因子負荷量の大きい順に並べる

# 因子負荷量
fa_loadings <- result$loadings[,] # 因子負荷量
# 因子負荷量の表
fa_loadings |> 
  round(3) |> # 小数点以下3ケタで四捨五入
  kable() |> # 表にする
  kable_styling(font_size =28)

# 因子負荷量の散布図
plot(fa_loadings, pch = "") # 散布図
text(fa_loadings[,1], fa_loadings[,2], row.names(fa_loadings)) # ラベ

