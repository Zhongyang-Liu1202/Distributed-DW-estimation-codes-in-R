rm(list = ls())
library(mvtnorm)

###################函数############################
guji <- function(t, n, K, p, error, gamma, beta, delta_a0, delta_a1, delta_a2, delta_a3){
  set.seed(t)
  N <- n * K
  
  rho <- 0.5
  cov_matrix <- matrix(NA, p, p)
  cov_matrix <- rho^(abs(col(cov_matrix) - row(cov_matrix)))
  X <- rmvnorm(N, rep(0, p), cov_matrix)
  Z <- rmvnorm(N, rep(0, p), cov_matrix)
  binary_cols <- which(((1:p - 1) %% 4) >= 2)
  X[, binary_cols] <- ifelse(X[, binary_cols] > 0, 1, 0)
  Z[, binary_cols] <- ifelse(Z[, binary_cols] > 0, 1, 0)
  X <- as.matrix(X)
  X[ ,1] <- 1
  Z <- as.matrix(Z)
  
  MSE <- function(para1, para2) {
    return(sum((para1 - para2)^2))
  }
  
  logis_loss <- function(y, X, delta) { #算权重
    loss <- ifelse(y == 1,
                   - y * log( exp(X %*% delta)/(1 + exp(X %*% delta)) ),
                   - (1 - y) * log(1 - (exp(X %*% delta)/(1 + exp(X %*% delta)) )))
    return(loss)
  }
  
  Loss <- function(delta) { 
    loss <- logis_loss(localD, MissLocalX, delta)
    return(mean(loss))
  }
  
  Weights <- function(D,missX, delta) {
    Prob2<- exp(missX %*% delta) / (1 + exp(missX %*% delta))
    W <- D / Prob2
    return(as.vector(W))
  }
  
  I <- rep((1:K), each = n)  
  
  Y <- numeric(0)
  
  for(k in 1:K){
    if (error == 1) {s <- X[I == k, ] %*% beta[k, ] + Z[I == k, ] %*% gamma + rnorm(n, sd = 1)}
    else if (error == 2){s <- X[I == k, ] %*% matrix(beta[k, ], ncol = 1) + Z[I == k, ] %*% gamma + rnorm(n, sd = 2)}
    else if (error == 3){s <- X[I == k, ] %*% matrix(beta[k, ], ncol = 1) + Z[I == k, ] %*% gamma + rnorm(n, sd = 4)}
    else if (error == 4){s <- X[I == k, ] %*% matrix(beta[k, ], ncol = 1) + Z[I == k, ] %*% gamma + (rexp(n, 1/3) - 3)}
    else if (error == 5){s <- X[I == k, ] %*% matrix(beta[k, ], ncol = 1) + Z[I == k, ] %*% gamma + ((X[I == k, 2] + Z[I == k, 1]) * rnorm(n, sd = 1))}
    Y <- rbind(Y, s)
  }
  
  I_a0 <- rep(delta_a0, each = n)
  I_a1 <- rep(delta_a1, each = n)
  I_a2 <- rep(delta_a2, each = n)
  I_a3 <- rep(delta_a3, each = n)
  eta_miss <- I_a0 + I_a1 * as.vector(Y) - I_a2 * X[, 2] + I_a3 * Z[, 2]
  
  Prob <- plogis(eta_miss)
  
  return(mean(Prob))
}
#######################参数估计##############
T <- 100
result <- numeric(0)
for(error in c(1)){
  for(K in c(5, 10, 20)){
    for(n in c(500, 1000, 2000)){
      result_group <- numeric(0)
      set.seed(666)
      p <- 8
      #beta_parameter <- 0.1 * (0:(p-1))
      #beta_base <- (log((1:K)))/2 - 1
      #beta <- as.data.frame(
      #  outer(beta_base, 1 + beta_parameter, FUN = "*")
      #)
      #colnames(beta) <- paste0("beta_", 1:p)
      #beta <- as.matrix(beta)
      #beta <- matrix(runif(p*K, -0.5, 0.5), nrow = K, ncol = p)
      gamma <- rep(c(-0.5, 0.5), length.out = p)
      beta_base_vector <- rep(c(-1, 1), length.out = p)
      beta_strength <- seq(
        from = 0.1,
        to = 1,
        length.out = K
      )
      beta <- sapply(1:K, function(i) {
        beta_base_vector * beta_strength[i]
      })
      beta <- t(beta)
      #delta_a0 <- runif(K, 1.7, 2) #20% missing rate
      #delta_a0 <- runif(K, 0.5, 0.8) #40% missing rate
      delta_a0 <- runif(K, 2.2, 2.5) #15% missing rate
      delta_a1 <- runif(K, 0.45, 0.55)
      delta_a2 <- runif(K, 0.2, 0.4)
      delta_a3 <- runif(K, 0.2, 0.4)
      for(t in 1:T){
        result_group <- rbind(result_group, guji(t, n, K, p, error, gamma, beta, delta_a0, delta_a1, delta_a2, delta_a3))
        cat("第", error, "种情况的误差", n, "个样本量", K,"组数据情况的第", t, "次模拟完毕",  "\n")
      }
      result <- rbind(result, apply(result_group, 2, mean))
      rownames(result)[nrow(result)] <- paste0("p=", p, "n=", n, "K=", K)
    }
  }  
}


#write.csv(result, file = "9.1 p=8 results with estimated weights.csv")
write.csv(result, file = "9.3 (40%-50% missing rate) p=8 results with estimated weights.csv")
#write.csv(result, file = "9.8 (20%-30% missing rate) p=8 results with estimated weights.csv")
