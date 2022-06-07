# 1 使用するライブラリ
install.packages("dplyr")
library(dplyr)



# 2 正規分布の点推定

## 2.1 データの作成(正規分布)
set.seed(10)
身長 <- rnorm(10, 170, 10) %>% round()
#身長 <- rnorm(100, 170, 10) %>% round()
x <- 身長
N <- length(x)

## 2.2 正規分布の不偏推定量
mu <- sum(x)/N
sigma1 <- sum((x-mu)^2)/(N-1)
mu ; sigma1

## 2.3 最尤推定の不偏推定量
sigma2 <- sum((x-mu)^2)/N
mu ; sigma2



# 3 ベルヌーイ分布の点推定(最尤推定量)

## 3.1 データ(ベルヌーイ分布)
set.seed(10)
x <- rbinom(10, 1, 0.5)
#x <- rbinom(100, 1, 0.5)
N <- length(x)

## 3.2 ベルヌーイ分布の最尤推定量
p <- sum(x)/N
