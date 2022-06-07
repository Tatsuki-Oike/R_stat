# 1 使用するライブラリとデータ作成

## 1.1 ライブラリ
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

## 1.2 データ
set.seed(10)
数学 <- rnorm(100, 30, 5) %>% round() # 数学~N(30,5)
英語 <- rnorm(100, 70, 10) %>% round() # 英語~N(70,10)
data <- data.frame(数学,英語)
head(data)



# 2 データの標準化と偏差値

## 2.1 データの標準化と偏差値
data_new <- data %>%
  mutate(数学z=scale(数学), 英語z=scale(英語)) %>% # データの標準化
  #mutate(数学z = (数学 - mean(数学))/sd(数学), 英語z = (英語 - mean(英語))/sd(英語)) %>%
  mutate(数学t=10*数学z+50, 英語t=10*英語z+50) # データを偏差値に変換
data_new %>% head()

## 2.2 平均と標準偏差
data_new %>% apply(2, mean) %>% round()
data_new %>% apply(2, sd) %>% round()



# 3 データの可視化

## 3.1 元データを正規分布と仮定
x <- seq(mean(data_new$数学)-3*sd(data_new$数学),mean(data_new$数学)+3*sd(data_new$数学),0.1)
y <- dnorm(x, mean(data_new$数学), sd(data_new$数学))
ggplot() + 
  geom_line(aes(x,y)) +
  theme_classic(base_family="HiraKakuPro-W3")+ # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(title="データに正規分布仮定") # タイトル

## 3.2 標準化したデータを正規分布と仮定
x <- seq(mean(data_new$数学z)-3*sd(data_new$数学z),mean(data_new$数学z)+3*sd(data_new$数学z),0.1)
y <- dnorm(x, mean(data_new$数学z), sd(data_new$数学z))
ggplot() + 
  geom_line(aes(x,y)) +
  theme_classic(base_family="HiraKakuPro-W3")+ # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(title="標準化したデータに正規分布仮定") # タイトル

## 3.3 偏差値に変換したデータを正規分布と仮定
x <- seq(mean(data_new$数学t)-3*sd(data_new$数学t),mean(data_new$数学t)+3*sd(data_new$数学t),0.1)
y <- dnorm(x, mean(data_new$数学t), sd(data_new$数学t))
ggplot() + 
  geom_line(aes(x,y)) +
  theme_classic(base_family="HiraKakuPro-W3")+ # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(title="偏差値にしたデータに正規分布仮定") # タイトル



# 4 データと標準化と偏差値の関係

## 4.1 標準化と偏差値変換の関数作成
zf <- function(x){
  z <- (x - mean(数学))/sd(数学)
  #z <- (x - mean(英語))/sd(英語)
  return(z)
}
zf(50) # 元データが50点のときの標準化得点

tf <- function(x){
  z <- (x - mean(数学))/sd(数学)
  #z <- (x - mean(英語))/sd(英語)
  t <- 10*z + 50
  return(t)
}
tf(50) # 元データが50点のときの偏差値

## 4.2 元データと標準化の関係
x <- seq(1,100,1)
z <- zf(x)
t <- tf(x)
ggplot() + 
  geom_line(aes(x,z)) +
  theme_classic(base_family="HiraKakuPro-W3")+ # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(x="元データ", y="標準化", title="元データと標準化の関係") # タイトル

## 4.3 元データと偏差値の関係
ggplot() + 
  geom_line(aes(x,t)) +
  theme_classic(base_family="HiraKakuPro-W3")+ # テーマと文字化け修正
  theme(text=element_text(size=30)) + # 文字サイズ
  labs(x="元データ", y="偏差値", title="元データと偏差値の関係") # タイトル
