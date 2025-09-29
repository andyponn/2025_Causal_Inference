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
