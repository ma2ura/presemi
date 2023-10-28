# ライブラリを読み込む
# install.packages("easypackages")
library("easypackages")
# install_packages("ggthemes", "knitr", "kableExtra", "plotly", "leaflet")
libraries(c("tidyverse", "ggthemes", "knitr",
  "kableExtra", "plotly", "leaflet"))

# グラフの見た目を設定
mystyle <- list( # mystyleとして設定を保存
  theme_few(), # ggthemesのテーマ
  theme(
    text = element_text(
    size = 12,  #  フォントサイズ
    family = "HiraKakuProN-W3" # for mac
    )
  )
)

# データをウェブサイトから読み込む
df <- read_csv("https://so-ichi.com/RandD_2022.csv")

# 中身チェック
glimpse(df)

# ファクター型に変


df <- df %>%
  mutate( # 変数の作成
    会社コード = as.factor(会社コード),
    企業名    = as.factor(企業名),
    連結基準   = as.factor(連結基準),
  # 決算期は "2000/03"という文字列なので，最初の4文字を取り出してファクター
    年度 = as.factor(substr(決算期, 1, 4)), # 年度を作成し，ファクター型
  # 日経業種コードは6ケタの数値だけれど，最初の1ケタは大分類，次の2ケタは中分類，最後の3ケタは小分類
    中分類 = as.factor(substr(日経業種コード, 2, 3)) # 日経中分類
  )

# 年度別平均売上高を計算
df %>%
  group_by(年度) %>% # 年度ごとに
  summarise( # 平均売上高を計算
    平均売上高 = mean(売上高, na.rm = TRUE)
  ) %>%
  kable() %>% # キレイな表
  kable_styling(font_size = 20) # 表のフォント

# 年度・産業別平均売上高を計算
df_ave <- df %>%
  group_by(年度, 中分類) %>% # 年度と中分類ごとに
  summarise( # 平均売上高を計算
    平均売上高 = mean(売上高, na.rm = TRUE)
  )

# df_aveを表にする。
df_ave %>%
  kable() %>% # キレイな表
  kable_styling(font_size = 20) # 表のフォント

# ggplotでグラフを作成
g <- ggplot(df_ave) + # df_aveを指定
  aes(x = 年度, # x軸を年度
      y = 平均売上高, # y軸を平均売上高
      group = 中分類, # グループを中分類
      color = 中分類 # 中分類で色分け
      ) +
  geom_line() + geom_point() # 線と点を追加
g <- g + xlab("年度") + ylab("平均売上高") # 軸ラベル
g <- g + mystyle # 上で指定したスタイルを適用
print(g) # 出力

# 動くグラフにする
ggplotly(g)
