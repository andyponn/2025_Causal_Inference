---
title: "因果分析hw3"
author: "陳葳芃 統計碩1 314657018"
date: "2025-10-14"
output: html_document
---


``` r
source("renv/activate.R")
```

```
## - Project '~/Documents/Project/2025_Causal_Inference' loaded. [renv 1.1.5]
## - The project is out-of-sync -- use `renv::status()` for details.
```

``` r
library(markdown)
```

## Part I: Estimation for continuous outcome. Show your code and answer by R Markdown.
### Q1-(A) According to the following models, please simulate a dataset with 1,000 observations by R, and show the first 5 observations.


``` r
# 設定隨機種子以便重現
set.seed(123)
# 生成 L
L <- runif(1000, min = 8, max = 16)

# 定義 expit 函數
expit <- function(x) {
  1 / (1 + exp(-x))
}

# 計算 p_A
p_A <- expit(6 - 0.5 * L)
# 根據 Bernoulli 生成 A
A <- rbinom(1000, size = 1, prob = p_A)
# 計算 E[Y|A,L]
EY <- 20 + 2*A + 0.1*L
# 根據正態分布生成 Y
Y <- rnorm(1000, mean = EY, sd = 1)
# 建立資料框
df <- data.frame(L = L, A = A, Y = Y)
# 顯示前 5 筆資料
head(df, 5)
```

```
##          L A        Y
## 1 10.30062 1 22.03426
## 2 14.30644 0 20.39069
## 3 11.27182 1 23.10920
## 4 15.06414 1 23.37424
## 5 15.52374 0 19.00303
```

### Q1-(B)Suppose L represents children’s age, A represents whether a child walks to school (A = 0) or picked up by parents (A = 1), and  represents children’s BMI. Please derive the estimate of causal effect based on standardization method under difference scale (both point and 95% CI).


``` r
lm.Y.AC = lm(Y ~ ., data = df)
newdata_df = df
newdata_df$A = 1
phi.1 = mean(predict(lm.Y.AC,newdata = newdata_df))
newdata_df$A = 0
phi.0 = mean(predict(lm.Y.AC,newdata = newdata_df))
print(phi.1 - phi.0)#point estimate by standardization method
```

```
## [1] 2.053833
```

``` r
B.lis = rep(NA, 3000)
for (b in 1:3000) {
  resample.df = df[sample(220,220,replace = T),]
  lm.Y.AC = lm(Y ~ ., data = resample.df)

  resample.df$A = 1
  phi.1 = mean(predict(lm.Y.AC,newdata = resample.df))

  resample.df$A = 0
  phi.0 = mean(predict(lm.Y.AC,newdata = resample.df))
  B.lis[b] = phi.1 - phi.0
  }
  quantile(B.lis,probs = c(0.025,0.975))#percentile bootstrap confidence interval of causal effect
```

```
##     2.5%    97.5% 
## 1.675347 2.196264
```

### Q1-(C)Please derive the estimate of causal effect based on regression-based estimator under difference scale (both point and 95% CI).


``` r
set.seed(123)
lm.Y.AC_Reg = lm(Y ~ ., data = df)
print(lm.Y.AC_Reg$coefficients[3])#point estimate by regression-based method
```

```
##        A 
## 2.053833
```

``` r
B_Reg.lis = rep(NA, 3000)
for (b in 1:3000) {
  resample.regdf = df[sample(220,220,replace = T),]
  lm.Y.AC_Reg = lm(Y ~ ., data = resample.regdf)

  B_Reg.lis[b] = lm.Y.AC_Reg$coefficients[3]
  }
quantile(B_Reg.lis,probs = c(0.025,0.975))#percentile bootstrap confidence interval of causal effect
```

```
##     2.5%    97.5% 
## 1.673502 2.213305
```

### Q1-(D)Please derive the estimate of causal effect based on IPW estimator under difference scale (both point and 95% CI).


``` r
lm.A.C = glm(A ~ .,data = df[,-2], family = binomial(link = "logit"))
phi.1 = mean((df$A == 1)*df$Y/predict(lm.A.C,type = "response"))
phi.0 = mean((df$A == 0)*df$Y/(1 - predict(lm.A.C,type = "response")))
print(phi.1 - phi.0)#IPW estimates of causal effect under difference scale
```

```
## [1] 0.3356125
```

``` r
B.lis = rep(NA, 3000)
for (b in 1:3000) {
  resample.df = df[sample(220,220,replace = T),]
  lm.A.C = glm(A ~ .,data = resample.df[,1:2], family = binomial(link = "logit"))
  phi.1 = mean((resample.df$A == 1)*resample.df$Y/predict(lm.A.C,type = "response"))
  phi.0 = mean((resample.df$A == 0)*resample.df$Y/(1 - predict(lm.A.C,type = "response")))

  B.lis[b] = phi.1 - phi.0
  }
quantile(B.lis, probs = c(0.025,0.975))#percentile bootstrap confidence interval of causal effect
```

```
##     2.5%    97.5% 
## 1.223886 4.740056
```

## Estimation for binary outcome. Show your code and answer by R Markdown.
### Q2-(A)


``` r
According to the following model, generate variable \(Y\):

$$
Y \sim Bern(p_y), \quad p_y = \text{expit}(-4 - A + 3L)
$$




set.seed(123)
expit <- function(x) 1 / (1 + exp(-x))
p_L <- expit(1)            # L
L <- rbinom(1000, 1, p_L)
p_A <- expit(-1 + 3*L)     # A 
A <- rbinom(1000, 1, p_A)
p_Y <- expit(-4 - A + 3*L) # Y
Y <- rbinom(1000, 1, p_Y)

df2 <- data.frame(L = L, A = A, Y = Y)
head(df2, 5)
```

```
## Error in parse(text = input): <text>:1:11: unexpected symbol
## 1: According to
##               ^
```

### Q2-(B) Suppose L represents a subject’s age (L = 0 if age < 65, L = 1 o.w.), A represents whether a subject is in the control group (A = 0) or the treatment group (A = 1), and  represents whether a subject is dead (Y = 1) or alive (Y = 0). Please derive the estimate of causal effect based on standardization method under odds ratio scale (both point and 95% CI).
### Q2-(C)Please derive the estimate of causal effect based on regression-based estimator under odds ratio scale (both point and 95% CI).
### Q2-(D)Please derive the estimate of causal effect based on IPW estimator under odds ratio scale (both point and 95% CI).


``` r
library(markdown)
knitr::spin("hw3.r", knit = TRUE)
```

```
## Warning: It seems you should call rmarkdown::render() instead of
## knitr::knit2html() because hw3.Rmd appears to be an R Markdown v2 document.
```

```
## 
## 
## processing file: hw3.Rmd
```

```
## 1/15                   
## 2/15 [unnamed-chunk-15]
## 3/15                   
## 4/15 [unnamed-chunk-16]
## 5/15                   
## 6/15 [unnamed-chunk-17]
## 7/15                   
## 8/15 [unnamed-chunk-18]
## 9/15                   
## 10/15 [unnamed-chunk-19]
## 11/15                   
## 12/15 [unnamed-chunk-20]
## 13/15                   
## 14/15 [unnamed-chunk-21]
## 15/15
```

```
## output file: hw3.md
```

```
## Warning in file.remove(outsrc): cannot remove file 'hw3.Rmd', reason 'No such
## file or directory'
```

