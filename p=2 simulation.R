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
  
  D <- rbinom(N, 1, prob = Prob)
  
  Miss_data <- cbind(rep(1, N), Y, X[ ,2], Z[ ,2])
  globalweights_IPW <- numeric(0)
  for(k in 1:K){
    localD <- D[I == k]
    MissLocalX <- Miss_data[I == k, ]
    delta_Local <- optim(par = c(2, 0.5, -1, 0.5), Loss)$par
    globalweights_IPW <- rbind(globalweights_IPW, Weights(localD, MissLocalX, delta_Local))  
  }
  
  V_IPW_1 <- matrix(0, nrow = ncol(Z), ncol = ncol(Z))
  V_IPW_2 <- matrix(0, nrow = ncol(Z), ncol = 1)
  for(k in 1:K){
    idx <- ((k - 1) * n + 1):(k * n)
    Xk <- X[idx, , drop = FALSE]
    Zk <- Z[idx, , drop = FALSE]
    Yk <- Y[idx]
    wk <- globalweights_IPW[k, ]
    Sxx <- crossprod(Xk, wk * Xk)
    Sxz <- crossprod(Xk, wk * Zk)
    Sxy <- crossprod(Xk, wk * Yk)
    Szz <- crossprod(Zk, wk * Zk)
    Szy <- crossprod(Zk, wk * Yk)
    V_IPW_1 <- V_IPW_1 + Szz - t(Sxz) %*% solve(Sxx, Sxz)
    V_IPW_2 <- V_IPW_2 + Szy - t(Sxz) %*% solve(Sxx, Sxy)
  } 
  gamma_IPW <- solve(V_IPW_1) %*% V_IPW_2
  ER_IPW_gamma <- MSE(gamma_IPW, gamma)
  
  ER_IPW_beta_set <- numeric(0)
  for(k in 1:K){
    wk <- globalweights_IPW[k, ]
    idx <- ((k - 1) * n + 1):(k * n)
    Xk <- X[idx, , drop = FALSE]
    Zk <- Z[idx, , drop = FALSE]
    Yk <- Y[idx]
    Sxx <- crossprod(Xk, wk * Xk)
    Sxz <- crossprod(Xk, wk * Zk)
    Sxy <- crossprod(Xk, wk * Yk)
    Szz <- crossprod(Zk, wk * Zk)
    Szy <- crossprod(Zk, wk * Yk)
    beta_IPW <- solve(Sxx) %*% Sxy - solve(Sxx) %*% Sxz %*% gamma_IPW
    ER_IPW_beta_set <- rbind(ER_IPW_beta_set, MSE(beta_IPW, beta[k, ]))
  }
  ER_IPW_beta <- apply(ER_IPW_beta_set, 2, mean)
  
  globalweights_CC <- D
  V_CC_1 <- matrix(0, nrow = ncol(Z), ncol = ncol(Z))
  V_CC_2 <- matrix(0, nrow = ncol(Z), ncol = 1)
  for(k in 1:K){
    idx <- ((k - 1) * n + 1):(k * n)
    Xk <- X[idx, , drop = FALSE]
    Zk <- Z[idx, , drop = FALSE]
    Yk <- Y[idx]
    wk <- D[I == k]
    Sxx <- crossprod(Xk, wk * Xk)
    Sxz <- crossprod(Xk, wk * Zk)
    Sxy <- crossprod(Xk, wk * Yk)
    Szz <- crossprod(Zk, wk * Zk)
    Szy <- crossprod(Zk, wk * Yk)
    V_CC_1 <- V_CC_1 + Szz - t(Sxz) %*% solve(Sxx, Sxz)
    V_CC_2 <- V_CC_2 + Szy - t(Sxz) %*% solve(Sxx, Sxy)
  }
  gamma_CC <- solve(V_CC_1) %*% V_CC_2 
  ER_CC_gamma <- MSE(gamma_CC, gamma)
  
  ER_CC_beta_set <- numeric(0)
  for(k in 1:K){
    wk <- D[I == k]
    idx <- ((k - 1) * n + 1):(k * n)
    Xk <- X[idx, , drop = FALSE]
    Zk <- Z[idx, , drop = FALSE]
    Yk <- Y[idx]
    Sxx <- crossprod(Xk, wk * Xk)
    Sxz <- crossprod(Xk, wk * Zk)
    Sxy <- crossprod(Xk, wk * Yk)
    Szz <- crossprod(Zk, wk * Zk)
    Szy <- crossprod(Zk, wk * Yk)
    beta_CC <- solve(Sxx) %*% Sxy - solve(Sxx) %*% Sxz %*% gamma_CC
    ER_CC_beta_set <- rbind(ER_CC_beta_set, MSE(beta_CC, beta[k, ]))
  }
  ER_CC_beta <- apply(ER_CC_beta_set, 2, mean)
  
  V_Com_1 <- matrix(0, nrow = ncol(Z), ncol = ncol(Z))
  V_Com_2 <- matrix(0, nrow = ncol(Z), ncol = 1)
  for(k in 1:K){
    idx <- ((k - 1) * n + 1):(k * n)
    Xk <- X[idx, , drop = FALSE]
    Zk <- Z[idx, , drop = FALSE]
    Yk <- Y[idx]
    Sxx <- crossprod(Xk, Xk)
    Sxz <- crossprod(Xk, Zk)
    Sxy <- crossprod(Xk, Yk)
    Szz <- crossprod(Zk, Zk)
    Szy <- crossprod(Zk, Yk)
    V_Com_1 <- V_Com_1 + Szz - t(Sxz) %*% solve(Sxx, Sxz)
    V_Com_2 <- V_Com_2 + Szy - t(Sxz) %*% solve(Sxx, Sxy)
  }
  gamma_Com <- solve(V_Com_1) %*% V_Com_2 
  ER_Com_gamma <- MSE(gamma_Com, gamma)
  
  ER_Com_beta_set <- numeric(0)
  for(k in 1:K){
    idx <- ((k - 1) * n + 1):(k * n)
    Xk <- X[idx, , drop = FALSE]
    Zk <- Z[idx, , drop = FALSE]
    Yk <- Y[idx]
    Sxx <- crossprod(Xk, Xk)
    Sxz <- crossprod(Xk, Zk)
    Sxy <- crossprod(Xk, Yk)
    Szz <- crossprod(Zk, Zk)
    Szy <- crossprod(Zk, Yk)
    beta_Com <- solve(Sxx) %*% Sxy - solve(Sxx) %*% Sxz %*% gamma_Com
    ER_Com_beta_set <- rbind(ER_Com_beta_set, MSE(beta_Com, beta[k, ]))
  }
  ER_Com_beta <- apply(ER_Com_beta_set, 2, mean)
  
  data <- cbind(X, Z)
  weight_all <- numeric(0)
  for(k in 1:K){
    weight_all <- c(weight_all, globalweights_IPW[k, ])
  }
  
  XtWX <- crossprod(data, weight_all * data)
  XtWY <- crossprod(data, weight_all * Y)
  beta_all_hat <- solve(XtWX, XtWY)
  ER_all_gamma <- MSE(beta_all_hat[(p+1):(2*p)], gamma)
  ER_all_beta_set <- numeric(0)
  for(k in 1:K){
    ER_all_beta_set <- c(ER_all_beta_set, MSE(beta_all_hat[1:p], beta[k, ]))
  }
  ER_all_beta <- mean(ER_all_beta_set)
  
  ER_group_gamma_set <- numeric(0)
  ER_group_beta_set <- numeric(0)
  for(k in 1:K){
    group_weights <- globalweights_IPW[k, ]
    beta_group_hat <- solve(t(data[I == k, ]) %*% (group_weights * data[I == k, ])) %*% t(data[I == k, ]) %*% (group_weights * Y[I == k])
    ER_group_beta_set <- c(ER_group_beta_set, MSE(beta_group_hat[1:p], beta[k, ]))
    ER_group_gamma_set <- c(ER_group_gamma_set, MSE(beta_group_hat[(p+1):(2*p)], gamma))
  }
  ER_group_gamma <- mean(ER_group_gamma_set)
  ER_group_beta <- mean(ER_group_beta_set)
  
  return(c(ER_IPW_gamma, ER_Com_gamma, ER_CC_gamma, ER_all_gamma, ER_group_gamma, ER_IPW_beta, ER_Com_beta, ER_CC_beta, ER_all_beta, ER_group_beta))
}
#######################参数估计##############
T <- 100
result <- numeric(0)
for(error in c(1)){
  for(K in c(5, 10, 20)){
    for(n in c(500, 1000, 2000)){
      result_group <- numeric(0)
      set.seed(666)
      p <- 2
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
      result <- rbind(result, sqrt(apply(result_group, 2, mean)))
      rownames(result)[nrow(result)] <- paste0("p=", p, "n=", n, "K=", K)
    }
  }  
}


#write.csv(result, file = "9.8 (20% missing rate) p=2 results with estimated weights.csv")
#write.csv(result, file = "9.8 (40% missing rate) p=2 results with estimated weights.csv")
write.csv(result, file = "9.8 (15% missing rate) p=2 results with estimated weights.csv")
