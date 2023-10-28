# ライブラリを読み込む
# install.packages("easypackages")
library("easypackages")
# install_packages("knitr", "kableExtra")
libraries(c("tidyverse", "knitr", "kableExtra"))

# tidyデータは縦型long
## rep(x, times = 3) # xを3回繰り返す
## 文字列は "" で囲む。
df_weather <- data.frame(
    place = c(rep("札幌", 3), rep("大阪", 3), rep("福岡", 3)),
    time = rep(c("6時", "12時", "18時"), 3),
    temp = c(12, 15, 13, 20, 24, 22, 23, 25, 25)
)

# 作成したdf_weatherを表にして表示
kable(df_weather) |> kable_styling(font_size = 24)

# 縦から横に
df_wide <- df_weather |>
    pivot_wider( # longをwideに
        names_from = time, # 横にする変数
        values_from = temp # 値
        )
# 作成したdf_wideを表にして表示
kable(df_wide) |> kable_styling(font_size = 24)

# 横から縦に
df_long <- df_wide |>
    pivot_longer( # wide を long に
        cols = c("6時", "12時", "18時"), # 縦にする変数
        names_to = "time", # 縦にした変数名
        values_to = "temp") # 値
kable(df_long) |> kable_styling(font_size = 24)
