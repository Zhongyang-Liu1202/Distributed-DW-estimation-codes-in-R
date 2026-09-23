rm(list = ls())

data <- read.csv("Data_collection.csv")
data_filter <- data[!(is.na(data$HW70)), ]
Y_index <- c("HW70")
X_index <- c("B19", "V106", "V190", "V012", "V701", "V040", "HV243A", "V171B", "V170", "HV216", "V151")
K_index <- c("V000")
W_index <- c("V005")
countries_index <- c("AO8", "CD8", "CM7", "KE8", "ML8", "MW8", "TZ8", "ZM8")
data_filter <- data_filter[data_filter$V000 %in% countries_index, ]

set.seed(666)
dat_analysis <- data_filter[ ,c(Y_index, X_index, K_index, W_index)]
colnames(dat_analysis) <- c("Y", paste0("X", rep(1:length(X_index))), "K", "W")
dat_analysis$D <- (apply(is.na(dat_analysis), 1, sum) == 0)*1
dat_analysis_complete <- dat_analysis[dat_analysis$D == 1, ]
countries_name <- names(table(dat_analysis_complete$K))
dat_analysis_complete$K <- match(dat_analysis_complete$K, countries_name)

K_index <- sort(unique(dat_analysis_complete$K))
X <- as.matrix(dat_analysis_complete[ ,paste0("X", rep(1:length(X_index)))])
Y <- as.matrix(dat_analysis_complete[ ,"Y"])
I <- as.matrix(dat_analysis_complete[ ,"K"])
W <- as.numeric(dat_analysis_complete[ ,"W"]) / 1000000
X <- cbind(rep(1, nrow(X)), X)

beta_hat_set <- numeric(0)
for(k in K_index){
  beta_hat_group <- solve(t(X[I == k, ]) %*% (W[I == k] * X[I == k, ])) %*% 
    t(X[I == k, ]) %*% (W[I == k] * Y[I == k])
  beta_hat_set <- rbind(beta_hat_set, c(beta_hat_group))
}
mu <- colMeans(beta_hat_set)
mu_initial <- mu
alpha <- sweep(beta_hat_set, MARGIN = 2, STATS = mu, FUN = "-")
alpha_initial <- alpha

L2_norm <- function(a){
  sqrt(sum((a^2)))
}

p <- ncol(X)
N <- nrow(X)
K <- length(K_index)
cross_validation_lambda <- function(lambda1, lambda2){
  mu <- mu_initial
  alpha <- alpha_initial
  itera <- 100
  for(i in 1:itera){
    for(g in 1:p){
      if(abs(mu[g]) < 1e-10){
        mu[g] <- 0
        next
      }
      s <- numeric(0)
      X_g <- numeric(0)
      W_g <- numeric(0)
      for(k in K_index){
        s <- c(s, Y[I == k] - X[I == k, -g] %*% mu[-g] - X[I == k, ] %*% alpha[k, ]) 
        X_g <- c(X_g, X[I == k, g])
        W_g <- c(W_g, W[I == k])
      }
      S_g <- (t(X_g) %*% (W_g * s)) / N
      if(abs(S_g) < (1 / abs(mu_initial[g])) * lambda1){mu[g] <- 0} else {mu[g] <- S_g / ((t(X_g) %*% (W_g * X_g))/N + ((1 / abs(mu_initial[g])) * lambda1) / abs(mu[g]))}
    }
    
    for(g in 1:p){
      if(L2_norm(alpha[-1, g]) < 1e-10){
        alpha[, g] <- 0
        next
      }
      s <- numeric(0)
      W_g <- numeric(0)
      X_group <- matrix(-X[I == 1, g], nrow = length(X[I == 1, g]), ncol = K-1)
      for(k in K_index){
        s <- c(s, Y[I == k] - X[I == k, ] %*% mu - X[I == k, -g] %*% alpha[k, -g])
        W_g <- c(W_g, W[I == k])
        nk <- sum(I == k)
        zero_matrix <- matrix(0, nrow = nk, ncol = K-1)
        if(k != 1){
          zero_matrix[ ,k-1] <- X[I == k, g]
          X_group <- rbind(X_group, zero_matrix)
        }
      }
      S_g <- (t(X_group) %*% (W_g * s)) / N
      if(L2_norm(S_g) < (1 / L2_norm(alpha_initial[2:K, g])) * lambda2){alpha_group <- rep(0, K-1)} else {alpha_group <- solve((t(X_group) %*% (W_g * X_group)) / N + (((1 / L2_norm(alpha_initial[2:K, g])) * lambda2)/L2_norm(alpha[-1, g])) * diag(1, ncol(X_group))) %*% S_g}
      alpha_group <- c(-sum(alpha_group), alpha_group)
      alpha[ ,g] <- alpha_group
    }
  }
  select_group <- (apply(abs(alpha), 2, mean) > 1e-8)*1
  loss <- 0
  for(k in 1:K){
    loss <- loss + t(Y[I == k] - X[I == k, ] %*% (mu+alpha[k, ])) %*% 
      (W[I == k] * (Y[I == k] - X[I == k, ] %*% (mu+alpha[k, ])))
  }
  loss <- 0.5 * loss
  mu_norm <- abs(mu)
  alpha_norm <- apply(alpha[-1, ], 2, L2_norm)
  alpha_initial_norm <- apply(alpha_initial[-1, ], 2, L2_norm)
  df <- sum(mu_norm > 1e-8) + sum(alpha_norm > 1e-8) + (K-2)*sum(alpha_norm/alpha_initial_norm)
  bic <- loss + log(N)*df
  return(bic)
}
lambda1_set <- c(0.01, 0.05, 0.1, 0.5, 1)
lambda2_set <- c(0.01, 0.05, 0.1, 0.5, 1)
cross_validation_results <- numeric(0)
for(lambda1 in lambda1_set){
  for(lambda2 in lambda2_set){
    cross_validation_results <- rbind(cross_validation_results, c(lambda1, lambda2, cross_validation_lambda(lambda1, lambda2)))
  }
}
best_index <- which.min(cross_validation_results[ ,3])
lambda1_best <- cross_validation_results[best_index, 1]
lambda2_best <- cross_validation_results[best_index, 2]

mu <- mu_initial
alpha <- alpha_initial
itera <- 100
for(i in 1:itera){
  for(g in 1:p){
    if(abs(mu[g]) < 1e-10){
      mu[g] <- 0
      next
    }
    s <- numeric(0)
    X_g <- numeric(0)
    W_g <- numeric(0)
    for(k in K_index){
      s <- c(s, Y[I == k] - X[I == k, -g] %*% mu[-g] - X[I == k, ] %*% alpha[k, ]) 
      X_g <- c(X_g, X[I == k, g])
      W_g <- c(W_g, W[I == k])
    }
    S_g <- (t(X_g) %*% (W_g * s)) / N
    if(abs(S_g) < (1 / abs(mu_initial[g])) * lambda1_best){mu[g] <- 0} else {mu[g] <- S_g / ((t(X_g) %*% (W_g * X_g))/N + ((1 / abs(mu_initial[g])) * lambda1_best) / abs(mu[g]))}
  }
  
  for(g in 1:p){
    if(L2_norm(alpha[-1, g]) < 1e-10){
      alpha[, g] <- 0
      next
    }
    s <- numeric(0)
    W_g <- numeric(0)
    X_group <- matrix(-X[I == 1, g], nrow = length(X[I == 1, g]), ncol = K-1)
    for(k in K_index){
      s <- c(s, Y[I == k] - X[I == k, ] %*% mu - X[I == k, -g] %*% alpha[k, -g])
      W_g <- c(W_g, W[I == k])
      nk <- sum(I == k)
      zero_matrix <- matrix(0, nrow = nk, ncol = K-1)
      if(k != 1){
        zero_matrix[ ,k-1] <- X[I == k, g]
        X_group <- rbind(X_group, zero_matrix)
      }
    }
    S_g <- (t(X_group) %*% (W_g * s)) / N
    if(L2_norm(S_g) < (1 / L2_norm(alpha_initial[2:K, g])) * lambda2_best){alpha_group <- rep(0, K-1)} else {alpha_group <- solve((t(X_group) %*% (W_g * X_group)) / N + (((1 / L2_norm(alpha_initial[2:K, g])) * lambda2_best)/L2_norm(alpha[-1, g])) * diag(1, ncol(X_group))) %*% S_g}
    alpha_group <- c(-sum(alpha_group), alpha_group)
    alpha[ ,g] <- alpha_group
  }
}
select_group <- (apply(abs(alpha), 2, mean) != 0)*1
names(select_group) <- c("Intercept", X_index)
select_group