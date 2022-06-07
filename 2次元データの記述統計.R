# 1 使用するライブラリとデータの作成

## 1.1 ライブラリ
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

## 1.2 データ
set.seed(10)
性別 <- sample(c("男性", "女性"), size=100, replace = TRUE) # 男性、女性のサンプルを100個
成績 <- sample(c("A", "B", "C"), size=100, replace = TRUE) # A, B, Cのサンプルを100個
偏差値 <- rnorm(100, 50, 10) %>% round(1) # 偏差値~N(50,10)
身長 <- rnorm(100, 170, 10) %>% round() # 偏差値~N(50,10)
data <- data.frame(性別, 成績, 偏差値, 身長)
head(data) # データ確認



# 2 クロス集計表(質的データの可視化)
table(data[,1:2])



# 3 散布図(量的データの可視化)
data %>%
  ggplot(aes(偏差値, 身長)) + # 軸の指定
  geom_point() + # 散布図
  theme_classic(base_family="HiraKakuPro-W3")+ # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(title="散布図") # タイトル



# 4 2次元データの指標

## 4.1 データの格納
x <- data$身長
y <- data$偏差値
N <- length(x) # データ数

## 4.2 2次元データの指標
不偏分散x <- var(x)
不偏分散y <- var(y)
不偏共分散 <- cov(x,y)
相関係数 <- cor(x,y)
不偏分散x ; 不偏分散y ; 不偏共分散 ; 相関係数

## 4.3 2次元データの指標(計算)
分散x <- sum((x-mean(x))^2)/N
分散y <- sum((y-mean(y))^2)/N
共分散 <- sum((x-mean(x))*(y-mean(y)))/N
相関係数2 <- 共分散/(sqrt(分散x)*sqrt(分散y))
分散x ; 分散y ; 共分散 ; 相関係数2

## 4.4 共分散の公式
共分散2 <- sum(x*y)/N - mean(x)*mean(y)
共分散 ; 共分散2
