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
## 1/5                  
## 2/5 [unnamed-chunk-5]
## 3/5                  
## 4/5 [unnamed-chunk-6]
## 5/5
```

```
## output file: hw3.md
```

```
## Warning in file.remove(outsrc): cannot remove file 'hw3.Rmd', reason 'No such
## file or directory'
```

