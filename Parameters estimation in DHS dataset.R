rm(list = ls())

data <- read.csv("Data_collection.csv")
data_filter <- data[!(is.na(data$HW70)), ]
Y_index <- c("HW70")
X_index <- c("V005", "B19", "V106", "V190", "V012", "V701", "V040", "HV243A", "V171B", "V170", "HV216", "V151")
K_index <- c("V000")

set.seed(666)
data_filter$B19 <- scale(data_filter$B19)
data_filter$V012 <- scale(data_filter$V012)
data_filter$V040 <- scale(data_filter$V040)
dat_analysis <- data_filter[ ,c(Y_index, X_index, K_index)]
colnames(dat_analysis) <- c("Y", paste0("X", rep(1:length(X_index))), "K")
dat_analysis$D <- (apply(is.na(dat_analysis), 1, sum) == 0)*1

countries_index <- sort(unique(dat_analysis$K))
X <- as.matrix(dat_analysis[ ,paste0("X", rep(1:length(X_index)))])
Y <- as.matrix(dat_analysis[ ,"Y"])
I <- as.matrix(dat_analysis[ ,"K"])
colnames(X) <- X_index
X_logis <- cbind(Y, X[ ,c("B19")], X[ ,c("V040")], X[ ,c("HV216")], X[ ,c("V151")], X[ ,c("V005")])

globalweights_IPW <- numeric(0)
Weights <- rep(NA, nrow(X_logis))
for(k in countries_index){
  X_logis_group <- X_logis[I == k, ]
  Y_group <- Y[I == k]
  D_group <- dat_analysis$D[I == k]
  data_logis_group <- cbind(D_group, X_logis_group)
  data_logis_group <- as.data.frame(data_logis_group)
  colnames(data_logis_group) <- c("D", "HW70", "B19", "V040", "HV216", "V151", "V005")
  fit <- glm(
    D ~ HW70 + B19 + V040 + HV216 + V151,
    data = data_logis_group,
    family = quasibinomial(link = "logit"),
    weights = V005 / 1000000
  )
  print(summary(fit)$coefficients)
  logis_X_group <- cbind(rep(1, nrow(data_logis_group)), data_logis_group$HW70, data_logis_group$B19, data_logis_group$V040, data_logis_group$HV216, data_logis_group$V151)
  pi <- exp(logis_X_group %*% summary(fit)$coefficients[ ,1]) / (1 + exp(logis_X_group %*% summary(fit)$coefficients[ ,1]))
  weights_group <- D_group / pi
  Weights[I == k] <- weights_group * data_logis_group$V005
}

data_set <- X
hete_index <- c("B19", "V040")
homo_index <- c("V106", "V190", "V012", "V701", "HV243A", "V171B", "V170", "HV216", "V151")
X <- data_set[ ,hete_index]
Z <- data_set[ ,homo_index]
X <- cbind(rep(1, nrow(X)), X)
colnames(X)[1] <- c("Intercept")

Z[which(is.na(Z[ ,c("V190")])) ,c("V190")] <- 0
Z[which(is.na(Z[ ,c("V701")])) ,c("V701")] <- 0

V_IPW_1 <- matrix(0, nrow = ncol(Z), ncol = ncol(Z))
V_IPW_2 <- matrix(0, nrow = ncol(Z), ncol = 1)
for(k in countries_index){
  Xk <- X[I == k, , drop = FALSE]
  Zk <- Z[I == k, , drop = FALSE]
  Yk <- Y[I == k]
  wk <- Weights[I == k]
  Sxx <- crossprod(Xk, wk * Xk)
  Sxz <- crossprod(Xk, wk * Zk)
  Sxy <- crossprod(Xk, wk * Yk)
  Szz <- crossprod(Zk, wk * Zk)
  Szy <- crossprod(Zk, wk * Yk)
  V_IPW_1 <- V_IPW_1 + Szz - t(Sxz) %*% solve(Sxx, Sxz)
  V_IPW_2 <- V_IPW_2 + Szy - t(Sxz) %*% solve(Sxx, Sxy)
} 
gamma_IPW <- solve(V_IPW_1) %*% V_IPW_2

beta_IPW <- numeric(0)
for(k in countries_index){
  wk <- Weights[I == k]
  Xk <- X[I == k, , drop = FALSE]
  Zk <- Z[I == k, , drop = FALSE]
  Yk <- Y[I == k]
  Sxx <- crossprod(Xk, wk * Xk)
  Sxz <- crossprod(Xk, wk * Zk)
  Sxy <- crossprod(Xk, wk * Yk)
  Szz <- crossprod(Zk, wk * Zk)
  Szy <- crossprod(Zk, wk * Yk)
  beta_IPW <- rbind(beta_IPW, solve(Sxx) %*% Sxy - solve(Sxx) %*% Sxz %*% gamma_IPW)
}

gamma_IPW
beta_IPW
