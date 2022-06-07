# 1 使用するライブラリ
install.packages("dplyr")
library(dplyr)



# 2 対応なしデータのt検定

## 2.1 データの作成(対応なし)
set.seed(10)
サイトA <- rnorm(50, 10000, 100) %>% round()
サイトB <- rnorm(50, 9900, 100) %>% round()
dataA <- data.frame(サイトA, サイトB) 
dataA %>% head()
dataA %>% apply(2,mean)

## 2.2 t検定(等分散性を仮定しない)
t.test(サイトA, サイトB, var.equal = FALSE)

## 2.3 t検定(等分散性を仮定)
t.test(サイトA, サイトB, var.equal = TRUE)
var.test(サイトA, サイトB)



# 3 対応ありデータのt検定

## 3.1 データの作成(対応あり)
set.seed(10)
中間テスト <- rnorm(50, 50, 10) %>% round()
期末テスト <- 中間テスト + rnorm(50, 10, 10) %>% round()
dataB <- data.frame(中間テスト, 期末テスト)
dataB %>% head()
dataB %>% apply(2,mean)

## 3.2 t検定
t.test(中間テスト, 期末テスト, paired = TRUE)



