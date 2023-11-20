library(tidyverse)
library(ggthemes)
library(scales)
setwd("/Users/soichi/Dropbox/R/presemi/data/")
 
df <- read_csv("presemi_20231120.csv")

df <- df |>
  rename(
    上場コード = "上場場部：コード",
    業種コード = "日経業種：コード",
    純資産 = "純資産合計／資本合計",
    売上高 = "売上高・営業収益［累計］",
    売上原価 = "売上原価・営業原価［累計］",
    販管費 = "販売費及び一般管理費［累計］",
    親会社利益 = "親会社株主に帰属する当期純利益（連結）／当期利益（単独）［累計］",
    広告宣伝費 = "【販管費】広告・宣伝費［累計］",
    従業員数 = "【販管費】人件費・福利厚生費［累計］",
    発行済株式数 = "期末発行済株式総数"
  )

df <- df |>
  mutate( # 新変数を作成
    年度 = factor(substr(決算期, 1, 4), ordered = TRUE),
    決算月 = factor(substr(決算期, 6, 7), ordered = TRUE),
    大分類 = as.factor(substr(業種コード, 1, 1)),
    中分類 = as.factor(substr(業種コード, 2, 3)),
    小分類 = as.factor(substr(業種コード, 4, 6))
  ) |>
  select(-決算期, -業種コード)

chu_level <- sort(unique(df$中分類)) # 中分類のカテゴリーを抽出
chu_name <- c("食品","繊維","パルプ・紙","化学工業","医薬品","石油","ゴム","窯業","鉄鉱業","非金属及び金属製品","機械","電気機器","造船","自動車・自動車部品","その他輸送用機器","精密機器","その他製造業","水産","鉱業","建設","商社","小売業","その他金融業","不動産","鉄道・バス","陸運","海運","空輸","倉庫・運輸関連","通信","電力","ガス","サービス業") # 33の産業名を格納
df <- df |> # コードに名前を割り当てる
  arrange(中分類) |> # 並び替え
  mutate( # 新変数の作成
    中分類 = factor( # 中分類をファクター型に変換
      中分類, levels = chu_level, labels = chu_name
    )
  )

df[is.na(df)] <- 0 # NAを0に変換

write_csv(df, "presemi08_full.csv") # データをcsvファイルとして保存

df |>
  filter(年度 >= 2010) |>
  write_csv("presemi08_2010.csv")

df |>
  filter(年度 >= 2018) |>
  write_csv("presemi08_2018.csv")


# 指定した項目の平均と分散と標準偏差を出力する関数を作成する。

mystat <- function(chu, item) {
  df |>
    filter(中分類 == chu) |>
    summarise(
      平均 = mean(!!sym(item), na.rm = TRUE),
      分散 = var(!!sym(item), na.rm = TRUE),
      標準偏差 = sd(!!sym(item), na.rm = TRUE)
    )
}

mystat("精密機器", "売上高")

# 特定企業の特定項目の時系列データを抽出し，折れ線グラフにする関数を作成する。

mytime <- function(name, item) {
  df |>
    filter(企業名称 == name) |>
    select(年度, !!sym(item)) |>
    ggplot(aes(x = 年度, y = !!sym(item), group = 1)) +
    geom_line() +
    labs(title = paste(name, item, sep = " "), x = "年度", y = item) +
    scale_y_continuous(labels = label_number(scale = 1/100, suffix = '億')) + # 単位
    theme_economist(base_family = "HiraKakuProN-W3") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}

mytime("トヨタ自動車", "売上高")

# 中分類と項目を引数としてヒストグラムを作成する関数を作る。





myhist <- function(df, chu, item) {
  df |>
    filter(中分類 == chu) |>
    ggplot(aes(x = !!sym(item))) +
    geom_histogram(bins = 20) +
    labs(title = paste(chu, item, sep = " "), x = item, y = "頻度") +
    theme_economist(base_family = "HiraKakuProN-W3")
}

myhist(df, "精密機器", "売上高")

