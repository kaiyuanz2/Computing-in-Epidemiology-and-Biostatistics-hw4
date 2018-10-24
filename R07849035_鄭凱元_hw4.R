# 練習5：請將Seizure1的資料整理成Seizure2的資料。(本題限用for迴圈回答)
Seizure1 <- read.csv('seizure.csv')
Seizure2 <- read.table('Seizure.txt')
head(Seizure1)
head(Seizure2)

n <- seq(1, length(Seizure1$y), 5)
for (i in n) {
  cat(format(Seizure1$trt[i]),format(Seizure1$y[i]), format(Seizure1$y[i+1]), 
      format(Seizure1$y[i+2]), format(Seizure1$y[i+3]), format(Seizure1$y[i+4]), 
      "\n", sep = " ")
}

# 練習6：請將Seizure2的資料整理成Seizure1的排列方式。(本題限用for迴圈回答)
n <- seq(1, length(Seizure2[, 1]))
for (i in n) {
  cat(format(Seizure2[i, 1]),format(Seizure2[i, 2]), "\n", sep = " ")
  cat(format(Seizure2[i, 1]),format(Seizure2[i, 3]), "\n", sep = " ")
  cat(format(Seizure2[i, 1]),format(Seizure2[i, 4]), "\n", sep = " ")
  cat(format(Seizure2[i, 1]),format(Seizure2[i, 5]), "\n", sep = " ")
  cat(format(Seizure2[i, 1]),format(Seizure2[i, 6]), "\n", sep = " ")
}

# Ex S1
# for迴圈
n <- 10
fac <- 1
for(i in 1:10){
  fac = fac * i
}
print(fac)

# while迴圈
n <- 10
i <- 1
fac <- 1
while(i <= n){
  fac <- fac * i
  i <- i+1
}

# Ex S2
hanoi <- function(n){
  if (n == 1){
    return(1) # n = 1 時次數為 1
  } else {
    return(2*hanoi(n-1)+1) # n > 1 時次數為 2*(n-1) +1
  }
}

hanoi(20)

# Ex S3
x <- matrix(c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000,
              3600, 4500, 10000, 8500, 3000, 10000, 1000, NA, 1200, 10000,
              3800, 5500, 9000, 6000, 6600, 3000, 9600, 6500, 8200, 8000,
              5000, 6600, 13000, 4500, 5000, NA, 10600, 9500, 7600, 6000,
              6600, 8000, 17000, 3000, 7000, 1000, 12600, 8500, 6000, NA),5,10, byrow = TRUE)  # 壓歲錢

apply(x, 1, mean, na.rm = TRUE) # row 平均數
apply(x, 2, mean, na.rm = TRUE) # col 平均數
apply(x, 1, median, na.rm = TRUE) # row 中位數
apply(x, 2, median, na.rm = TRUE) # col 中位數
apply(x, 1, max, na.rm = TRUE) # row 最大值
apply(x, 2, max, na.rm = TRUE) # col 最大值
apply(x, 1, min, na.rm = TRUE) # row 最小值
apply(x, 2, min, na.rm = TRUE) # col 最小值

# Ex S5: (1) 將課程網站上的seizure.csv裡的trt, age, ltime當成解釋變數，y當成反應變數，求線性迴歸係數值，並需與R的內建函數lm結果比對。
i <- rep(1, length(Seizure1$trt))
X <- cbind(i, Seizure1$trt, Seizure1$age, Seizure1$ltime) # 建立design matrix
DM <- as.matrix(X) # 將資料轉成矩陣型式
beta <- solve(t(DM)%*%DM)%*%(t(DM)%*%Seizure1$y) 
fit <- lm(y ~ trt+age+ltime, data = Seizure1)
summary(fit)

# (2) 承上小題，請自行計算出殘差，並需與lm計算的殘差作比對。
res <- Seizure1$y - DM%*%beta

plot(resid(fit), xlab = "y", ylab = "residual", frame.plot = F)
points(res, col = "red", pch = 3)
legend("topleft", legend = c("lm計算的殘差", "代公式算出"), pch = c(1, 3),col = c(1, 2))

# Ex S7
A <- matrix(c(2, -1, 1, 4, -1, 3, 2, -3, 2), 3, 3, byrow = T)
b <- matrix(c(3, 2, 1), 3, 1)
solve(A,b)

# Ex S8   SINGULAR!!!
A <- matrix(c(2, -1, 1, 4, -4, 3, 2, -3, 2), 3, 3, byrow = T)
b <- matrix(c(3, 2, 1), 3, 1)
solve(A,b)

# Ex S9
y <- as.matrix(Seizure1$y, length(y), 1)
I <- matrix(0, length(y), length(y))
diag(I) <- 1
n <- matrix(1/length(y), length(y), length(y))
A <- I - n

t(y)%*%A%*%y

fit <- lm(Seizure1$y ~ ., data = Seizure1)
sum(anova(fit)[, 2]) # SSTO
