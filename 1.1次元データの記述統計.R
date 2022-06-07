# 1 使用するライブラリとデータの作成

## 1.1 ライブラリ
install.packages("dplyr") # 一度もインストールしていない場合
install.packages("ggplot2") # 一度もインストールしていない場合
install.packages("psych") # 一度もインストールしていない場合
library(dplyr)
library(ggplot2)
library(psych)

## 1.2 データ
set.seed(10)
性別 <- sample(c("男性", "女性"), size=100, replace = TRUE) # 男性、女性のサンプルを100個
成績 <- sample(c("A", "B", "C"), size=100, replace = TRUE) # A, B, Cのサンプルを100個
偏差値 <- rnorm(100, 50, 10) %>% round(1) # 偏差値~N(50,10)
身長 <- rnorm(100, 170, 10) %>% round() # 偏差値~N(50,10)
data <- data.frame(性別, 成績, 偏差値, 身長)
head(data) # データ確認



# 2 棒グラフ(質的データの可視化)
data %>%
  ggplot(aes(性別)) + # 軸の設定
  # ggplot(aes(成績)) + # 軸の設定
  geom_bar() + # 棒グラフ
  theme_classic(base_family = "HiraKakuPro-W3") + # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(title="棒グラフ") # タイトル



# 3 ヒストグラム(量的データの可視化)

## 3.1 身長
data %>%
  ggplot(aes(身長))+ # 軸の設定
  geom_histogram(binwidth = 10, col="white") + # ヒストグラムと幅(binwidth)と色(col)
  theme_classic(base_family = "HiraKakuPro-W3") + # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(title="ヒストグラム") # タイトル

## 3.2 偏差値
data %>%
  ggplot(aes(偏差値))+ # 軸の設定
  geom_histogram(breaks=seq(0,100,10), col="white") + # ヒストグラムと幅(breaks)と色(col)
  theme_classic(base_family = "HiraKakuPro-W3") + # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(title="ヒストグラム") # タイトル



# 4 1次元データの代表値

## 4.1 1次元データの代表値
平均値 <- mean(data$身長)
中央値 <- median(data$身長)
最頻値 <- data$身長 %>% table() %>% which.max() %>% names()
四分位点 <- quantile(data$身長)
パーセンタイル <- quantile(data$身長, c(0.05,0.95))
平均値 ; 中央値 ; 最頻値 ; 四分位点 ; パーセンタイル 

## 4.2 平均
平均値2 <- sum(data$身長)/nrow(data) # 平均値の計算
平均値 ; 平均値2



# 5 1次元データのばらつき

## 5.1 1次元データのばらつき
不偏分散 <- var(data$身長)
標準偏差 <- sd(data$身長)
レンジ <- max(data$身長) - min(data$身長)
四分位範囲 <- (quantile(data$身長)[4]- quantile(data$身長)[2])/2
不偏分散 ; 標準偏差 ; レンジ ; 四分位範囲[[1]]

## 5.2 不偏分散
不偏分散2 <- sum((data$身長-mean(data$身長))^2)/(nrow(data)-1) # 不偏分散の計算
不偏分散 ; 不偏分散2

## 5.3 分散の公式
分散 <- sum((data$身長-mean(data$身長))^2)/nrow(data) # 分散の計算
分散2 <- mean(data$身長^2) - mean(data$身長)^2 # 分散の公式
分散 ; 分散2



# 6 関数
summary(data)
describe(data)



# 7 apply関数
data[,3:4] %>% head()
data[,3:4] %>% apply(2,mean) # 各列の平均値
data[,3:4] %>% apply(2,sd) # 各列の標準偏差
