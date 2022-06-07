# 1 使用するライブラリ
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)



# 2 相関係数の検定

## 2.1 データ作成
set.seed(100)
数学 <- rnorm(40, 50, 20) %>% round()
英語 <- 数学-20 + rnorm(40, 5, 30)  %>% round()
data <- data.frame(数学, 英語)%>%
  filter(0<数学&数学<100) %>%
  filter(0<英語&英語<100)
data %>% head()

## 2.2 データの可視化
data %>%
  ggplot(aes(数学, 英語)) +
  geom_point() +
  theme_classic(base_family = "HiraKakuPro-W3") + 
  theme(text=element_text(size=40))+
  labs(title="数学と英語の関係") +
  lims(x=c(0,100), y=c(0,100))

## 2.3 相関係数の分析
cor.test(data$数学, data$英語)



# 3 独立性の検定

## 3.1 データ作成
set.seed(100)
N <- 30
コロナ <- c("未感染", "感染")
住居 <- c(rep("東京", N),rep("大阪", N),rep("沖縄", N))
コロナ <- c(コロナ[rbinom(N,1,0.7)+1], コロナ[rbinom(N,1,0.5)+1], コロナ[rbinom(N,1,0.5)+1])
data2 <- data.frame(コロナ, 住居)
data2 %>% head()

## 3.2 データの可視化
table(data2$コロナ, data2$住居)

## 3.3 独立性の分析
chisq.test(table(data2$コロナ, data2$住居))

