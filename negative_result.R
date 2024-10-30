##########
library(igraph)
library(tidyverse)
library(stringdist)
# library(dplyr)
if (!requireNamespace("Matrix", quietly = TRUE)) {
  install.packages("Matrix")
}
library(Matrix)
##########
# 加载必要的包
if (!requireNamespace("quadprog", quietly = TRUE)) {
  install.packages("quadprog")
}
library(quadprog)






# r1 <- 0.5


n <- 1000


#generate pi:


pscore1 <-  runif(n, 0.1, 0.9)
# pscore1 <- rbinom(n, size = 1, prob = 0.2)
# pscore1 <- sample(c(0.2, 0.8), size = n, replace = TRUE, prob = c(0.5, 0.5))
# pscore1 <- rep(0.5, n)


pscore0 <- 1 - pscore1


# X <- rt(n, 3)
# X <- rnorm(n) * 5


C <- 0.5 * rnorm(n) 
# C <- runif(n, 0.1, 0.9)
# C <- rep(10, n)

Y_1 <- pscore1/pscore0 * C 
Y_0 <- -C


errors <- runif(n, 1, 5)
errors <- rnorm(n) 


X <- C 
X <- scale(X)




data1 <- read.table("~/regression_interference_server/syn_total_new/linear_parameter_X_counter.txt", header = FALSE)
data2 <- read.table("~/regression_interference_server/syn_total_new/linear_parameter_Y0_counter.txt", header = FALSE)
data3 <- read.table("~/regression_interference_server/syn_total_new/linear_parameter_Y1_counter.txt", header = FALSE)
data4 <- read.table("~/regression_interference_server/syn_total_new/linear_parameter_noise_counter.txt", header = FALSE)
data5 <- read.table("~/regression_interference_server/syn_total_new/linear_parameter_ps1_counter.txt", header = FALSE)
data6 <- read.table("~/regression_interference_server/syn_total_new/linear_parameter_ps0_counter.txt", header = FALSE)

X <- as.matrix(data1[, -1])  # 提取向量
Y_0 <- as.matrix(data2[, -1])  # 提取向量
Y_1 <- as.matrix(data3[, -1])  # 提取向量
errors <- as.vector(data4[, -1])   # 提取向量
pscore1 <-  as.vector(data5[, -1])   # 提取向量
pscore0 <-  as.vector(data6[, -1])   # 提取向量



get_Y <- function(Z){
  
  
  Y <- Z * Y_1 + (1-Z) * Y_0 + 0.5 * errors
  return(Y)
  
  
}

















tau <- map_dbl(1:10000, ~{
  Z <- rbinom(n, size = 1, prob = pscore1); Y <- get_Y(Z)
  T_vec <- Z; D <- Y*T_vec/pscore1-Y*(1-T_vec)/pscore0
  return(mean(D))
}) %>% mean()


tau


tau_haj <- map_dbl(1:10000, ~{
  Z <- rbinom(n, size = 1, prob = pscore1); Y <- get_Y(Z)
  T_vec <- Z; D <- Y*T_vec/(pscore1*mean(T_vec/pscore1))-Y*(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  # w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  return(mean(D))
}) %>% mean()




tau_haj




sum <- c()


##################################################################################################################################bn = 3
sim_res<- map_dfr(1:500, ~{
  A <- diag(n)
  sum <- c(sum,1); print("sum:"); print(length(sum))
  Z <- rbinom(n, size = 1, prob = pscore1); Y <- get_Y(Z); X_aug <- X
  T_vec <- (Z)
  w <- T_vec/pscore1-(1-T_vec)/pscore0
  w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  #add definition:
  w_1 <- T_vec/pscore1
  w_0 <- (1-T_vec)/pscore0
  w_haj_1 <- T_vec/(pscore1*mean(T_vec/pscore1))
  w_haj_0 <- (1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  
  #######################################Leung#################################################################  
  D <- Y*w
  D_haj <- Y*w_haj
  
  Leung <- mean(D); 
  var_Leung <- t(D-Leung)%*%A%*%(D-Leung)/n^2 %>% as.vector();
  var_Leung_naive <- t(D-Leung)%*%diag(n)%*%(D-Leung)/n^2 %>% as.vector()
  coverage_Leung <- abs(Leung-tau)<=qnorm(0.975)*sqrt(var_Leung)
  coverage_Leung_naive <- abs(Leung-tau)<=qnorm(0.975)*sqrt(var_Leung_naive)
  
  #########################################Leung#####################################################################
  
  
  
  
  
  
  
  ############################################Ours_X_F##########################################################################################
  
  X_db <- X  #it is n*4;  n*1,  n*4
  D_2 <- (X_db * w)
  # D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  
  
  
  hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
  # hbeta_2_haj <- drop(hbeta_2_haj)
  # hbeta_2_haj <- 0
  Ours_X_haj <- mean((Y-(X_db) %*% hbeta_2_haj)*w_haj)
  # Ours_X_haj <- mean((Y)*w_haj)
  var_Ours_X_haj <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  var_Ours_X_haj_naive <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%diag(n) %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  coverage_Ours_X_haj <- abs(Ours_X_haj - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj)
  coverage_Ours_X_haj_naive <- abs(Ours_X_haj - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_naive)
  
  
  
  
  
  
  ######################################################Ours_X_lin##################################################################
  #Moreover, we use the lin's method!  X+Haj+lin；lin的第四种
  X_db <- X #it is n*4;  n*1,  n*4
  X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  #here we use...
  # X_db <- cbind(X * pscore1, X * pscore0)
  D_2 <- (X_db * w)
  # D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  
  V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%V) #here we should use the new variance estimator:
  
  # quadratic_function <- function(hbeta) {
  #   V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  #   result <- t(V- D_2%*%hbeta ) %*% A %*% (V- D_2%*%hbeta)/n^2 %>% as.vector()
  #   result<- sqrt(result)
  #   return(result)
  # }
  # result <- optim( vector("logical", length = 2    ) * 1,          quadratic_function)
  # # # 输出最优值和最优位置
  # cat("最小值：", result$value, "\n")
  # hbeta_2_haj <- result$par
  
  
  # hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
  Ours_X_haj_lin <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  var_Ours_X_haj_lin <- t(V - D_2%*%hbeta_2_haj  ) %*%A %*% (V - D_2%*%hbeta_2_haj )/n^2 %>% as.vector()
  var_Ours_X_haj_lin_naive <- t(V - D_2%*%hbeta_2_haj  ) %*%diag(n) %*% (V - D_2%*%hbeta_2_haj )/n^2 %>% as.vector()
  coverage_Ours_X_haj_lin <- abs(Ours_X_haj_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_lin)
  coverage_Ours_X_haj_lin_naive <- abs(Ours_X_haj_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_lin_naive)
  
  
  ###################################################Ours_X_lin#############################################################################
  
  
  ###################################################Ours_X_Phi############################################################
  #Moreover, we use the lin's method!  X+Haj+lin；lin的第四种
  X_db <- X #it is n*4;  n*1,  n*4
  X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  #here we use...
  X_db <- cbind(X * pscore1, X * pscore0)
  D_2 <- (X_db * w)
  # D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  
  V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%V) #here we should use the new variance estimator:
  
  # quadratic_function <- function(hbeta) {
  #   V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  #   result <- t(V- D_2%*%hbeta ) %*% A %*% (V- D_2%*%hbeta)/n^2 %>% as.vector()
  #   result<- sqrt(result)
  #   return(result)
  # }
  # result <- optim( vector("logical", length = 2    ) * 1,          quadratic_function)
  # # # 输出最优值和最优位置
  # cat("最小值：", result$value, "\n")
  # hbeta_2_haj <- result$par
  
  
  # hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
  Ours_X_haj_lin_phi <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  var_Ours_X_haj_lin_phi <- t(V - D_2%*%hbeta_2_haj  ) %*%A %*% (V - D_2%*%hbeta_2_haj )/n^2 %>% as.vector()
  var_Ours_X_haj_lin_naive_phi <- t(V - D_2%*%hbeta_2_haj  ) %*%diag(n) %*% (V - D_2%*%hbeta_2_haj )/n^2 %>% as.vector()
  coverage_Ours_X_haj_lin_phi <- abs(Ours_X_haj_lin_phi - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_lin_phi)
  coverage_Ours_X_haj_lin_naive_phi <- abs(Ours_X_haj_lin_phi - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_lin_naive_phi)
  ###################################################Ours_X_Phi############################################################
  
  
  
  
  #########################Gao_L####################################################
  # lm_haj <- lm(Y~1+T_vec+X+T_vec:X, w = T_vec/pscore1+(1-T_vec)/pscore0)
  # w <- T_vec/pscore1+(1-T_vec)/pscore0
  # e_haj <- lm_haj %>% resid(); Gao_L <- lm_haj %>% coef() %>% .[2];
  # C_haj <- cbind(1,T_vec, T_vec*X)
  # var_Gao_L <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # # var_Gao_L <-
  # var_Gao_L_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%diag(n)%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # coverage_Gao_L <- abs(Gao_L-tau)<=qnorm(0.975)*sqrt(var_Gao_L)
  # coverage_Gao_L_naive <- abs(Gao_L-tau)<=qnorm(0.975)*sqrt(var_Gao_L_naive)
  
  # Gao_L_plus <- Gao_L
  # var_Gao_L_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # var_Gao_L_naive_plus <- var_Gao_L_naive
  # coverage_Gao_L_plus <- abs(Gao_L-tau)<=qnorm(0.975)*sqrt(var_Gao_L_plus)
  # coverage_Gao_L_naive_plus <- coverage_Gao_L_naive
  
  lm_haj <- lm(Y~1+T_vec+X+T_vec:X, w = T_vec/pscore1+(1-T_vec)/pscore0)
  e_haj <- lm_haj %>% resid()
  Gao_L <- lm_haj %>% coef() %>% .[2];
  # C_haj <- cbind(1,T_vec, T_vec*X)
  # var_Gao_L <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  X_db <- X #it is n*4;  n*1,  n*4
  X_db <- cbind(X_db * (1-T_vec), X_db * (T_vec))
  # X_db <- cbind(X * (1-pscore0), X * (pscore0))
  hbeta_2_haj <- c(lm_haj %>% coef() %>% .[3],   lm_haj %>% coef() %>% .[3] + lm_haj %>% coef() %>% .[4]   )
  # Ours_X_haj <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  w = T_vec/pscore1 - (1-T_vec)/pscore0
  D_2 <- X_db * w
  var_Gao_L <- t(D- D_2%*%hbeta_2_haj  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  var_Gao_L_naive <- t(D- D_2%*%hbeta_2_haj  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%diag(n) %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # var_Gao_L_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%diag(n)%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  coverage_Gao_L <- abs(Gao_L-tau)<=qnorm(0.975)*sqrt(var_Gao_L)
  coverage_Gao_L_naive <- abs(Gao_L-tau)<=qnorm(0.975)*sqrt(var_Gao_L_naive)
  #########################Gao_L####################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############################################################################################################
  
  
  
  
  
  
  
  
  #########################Gao_F####################################################
  
  # lm_haj <- lm(Y~1+T_vec+X, w = T_vec/pscore1+(1-T_vec)/pscore0)
  # w <- T_vec/pscore1+(1-T_vec)/pscore0
  # e_haj <- lm_haj %>% resid(); Gao_F <- lm_haj %>% coef() %>% .[2]
  # C_haj <- cbind(1,T_vec,X)
  # var_Gao_F <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # var_Gao_F_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%diag(n)%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # coverage_Gao_F <- abs(Gao_F-tau)<=qnorm(0.975)*sqrt(var_Gao_F)
  # coverage_Gao_F_naive <- abs(Gao_F-tau)<=qnorm(0.975)*sqrt(var_Gao_F_naive)
  
  # Gao_F_plus <- Gao_F
  # var_Gao_F_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # var_Gao_F_naive_plus <- var_Gao_F_naive
  # coverage_Gao_F_plus <- abs(Gao_F-tau)<=qnorm(0.975)*sqrt(var_Gao_F_plus)
  # coverage_Gao_F_naive_plus <- coverage_Gao_F_naive 
  
  lm_haj <- lm(Y~1+T_vec+X, w = T_vec/pscore1+(1-T_vec)/pscore0)
  e_haj <- lm_haj %>% resid()
  Gao_F <- lm_haj %>% coef() %>% .[2]
  # C_haj <- cbind(1,T_vec,X)
  X_db <- X
  w <- T_vec/pscore1 - (1-T_vec)/pscore0
  D_2 <- X_db * w
  hbeta_2_haj <- (lm_haj %>% coef() %>% .[3]  )
  var_Gao_F <- t(D- D_2 * hbeta_2_haj  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2 * hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  var_Gao_F_naive <- t(D- D_2 * hbeta_2_haj  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%diag(n) %*% (D- D_2 * hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # var_Gao_L_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%diag(n)%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  coverage_Gao_F <- abs(Gao_F-tau)<=qnorm(0.975)*sqrt(var_Gao_F)
  coverage_Gao_F_naive <- abs(Gao_F-tau)<=qnorm(0.975)*sqrt(var_Gao_F_naive)
  #########################Gao_F####################################################
  
  
  
  
  ##########################Gao###############################################################################################
  
  lm_haj <- lm(Y~1+T_vec, w = T_vec/pscore1+(1-T_vec)/pscore0)
  w <- T_vec/pscore1+(1-T_vec)/pscore0
  e_haj <- lm_haj %>% resid(); Gao <- lm_haj %>% coef() %>% .[2]
  C_haj <- cbind(1,T_vec)
  Gao_plus <- Gao
  # var_Gao <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # var_Gao_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%diag(n)%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # coverage_Gao <- abs(Gao-tau)<=qnorm(0.975)*sqrt(var_Gao)
  # coverage_Gao_naive <- abs(Gao-tau)<=qnorm(0.975)*sqrt(var_Gao_naive)
  
  # Gao_plus <- Gao
  # var_Gao_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  # var_Gao_naive_plus <- var_Gao_naive 
  # coverage_Gao_plus <- abs(Gao-tau)<=qnorm(0.975)*sqrt(var_Gao_plus)
  # coverage_Gao_naive_plus <- coverage_Gao_naive
  
  var_Gao <- t(D  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  var_Gao_naive <- t(D - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%diag(n) %*% (D  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # var_Gao_L_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%diag(n)%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  coverage_Gao <- abs(Gao-tau)<=qnorm(0.975)*sqrt(var_Gao)
  coverage_Gao_naive <- abs(Gao-tau)<=qnorm(0.975)*sqrt(var_Gao_naive)
  
  #重新按照我们的方法试验：
  # beta_gao_f <- matrix(cbind( lm_haj %>% coef() %>% .[3],lm_haj %>% coef() %>% .[4],lm_haj %>% coef() %>% .[5],  lm_haj %>% coef() %>% .[6],lm_haj %>% coef() %>% .[7],lm_haj %>% coef() %>% .[8] ))
  # var_Gao_naive <- t(D  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # var_Gao_naive_plus <- t(D -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # var_Gao_naive_plus_naive <- t(D  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%diag(n) %*% (D -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # coverage_Gao_naive_plus <- abs(Gao-tau)<=qnorm(0.975)*sqrt(var_Gao_naive_plus)
  # coverage_Gao_naive_plus_naive <- abs(Gao-tau)<=qnorm(0.975)*sqrt(var_Gao_naive_plus_naive)
  
  ##############################Gao######################################################################################
  
  
  
  
  
  
  
  # return(tibble(Leung, Gao, Gao_F, Gao_L,  Ours_X_ht, Ours_G_ht,Ours_X_ht_lin, Ours_G_ht_lin, Ours_X_haj, Ours_G_haj,  Ours_X_haj_lin,   Ours_G_haj_lin, 
  #               var_Leung, var_Gao, var_Gao_F, var_Gao_L, var_Ours_X_ht, var_Ours_G_ht,  var_Ours_X_ht_lin, var_Ours_G_ht_lin, var_Ours_X_haj, var_Ours_G_haj, var_Ours_X_haj_lin,  var_Ours_G_haj_lin, 
  #               coverage_Leung, coverage_Gao, coverage_Gao_F, coverage_Gao_L,  coverage_Ours_X_ht, coverage_Ours_G_ht,  coverage_Ours_X_ht_lin, coverage_Ours_G_ht_lin, coverage_Ours_X_haj, coverage_Ours_G_haj,coverage_Ours_X_haj_lin, coverage_Ours_G_haj_lin,
  #               var_Leung_naive, var_Gao_naive, var_Gao_F_naive, var_Gao_L_naive, var_Ours_X_ht_naive, var_Ours_G_ht_naive,  var_Ours_X_ht_lin_naive, var_Ours_G_ht_lin_naive, var_Ours_X_haj_naive, var_Ours_G_haj_naive, var_Ours_X_haj_lin_naive,  var_Ours_G_haj_lin_naive, 
  #               coverage_Leung_naive, coverage_Gao_naive, coverage_Gao_F_naive, coverage_Gao_L_naive,  coverage_Ours_X_ht_naive, coverage_Ours_G_ht_naive,  coverage_Ours_X_ht_lin_naive, coverage_Ours_G_ht_lin_naive, coverage_Ours_X_haj_naive, coverage_Ours_G_haj_naive,coverage_Ours_X_haj_lin_naive, coverage_Ours_G_haj_lin_naive, 
  #               
  #               
  #               Leung_plus, Gao_plus, Gao_F_plus, Gao_L_plus,  Ours_X_ht_plus, Ours_G_ht_plus,Ours_X_ht_lin_plus, Ours_G_ht_lin_plus, Ours_X_haj_plus, Ours_G_haj_plus,  Ours_X_haj_lin_plus,   Ours_G_haj_lin_plus, 
  #               var_Leung_plus, var_Gao_plus, var_Gao_F_plus, var_Gao_L_plus, var_Ours_X_ht_plus, var_Ours_G_ht_plus,  var_Ours_X_ht_lin_plus, var_Ours_G_ht_lin_plus, var_Ours_X_haj_plus, var_Ours_G_haj_plus, var_Ours_X_haj_lin_plus,  var_Ours_G_haj_lin_plus, 
  #               coverage_Leung_plus, coverage_Gao_plus, coverage_Gao_F_plus, coverage_Gao_L_plus,  coverage_Ours_X_ht_plus, coverage_Ours_G_ht_plus,  coverage_Ours_X_ht_lin_plus, coverage_Ours_G_ht_lin_plus, coverage_Ours_X_haj_plus, coverage_Ours_G_haj_plus,coverage_Ours_X_haj_lin_plus, coverage_Ours_G_haj_lin_plus,
  #               var_Leung_naive_plus, var_Gao_naive_plus, var_Gao_F_naive_plus, var_Gao_L_naive_plus, var_Ours_X_ht_naive_plus, var_Ours_G_ht_naive_plus,  var_Ours_X_ht_lin_naive_plus, var_Ours_G_ht_lin_naive_plus, var_Ours_X_haj_naive_plus, var_Ours_G_haj_naive_plus, var_Ours_X_haj_lin_naive_plus,  var_Ours_G_haj_lin_naive_plus, 
  #               coverage_Leung_naive_plus, coverage_Gao_naive_plus, coverage_Gao_F_naive_plus, coverage_Gao_L_naive_plus,  coverage_Ours_X_ht_naive_plus, coverage_Ours_G_ht_naive_plus,  coverage_Ours_X_ht_lin_naive_plus, coverage_Ours_G_ht_lin_naive_plus, coverage_Ours_X_haj_naive_plus, coverage_Ours_G_haj_naive_plus,coverage_Ours_X_haj_lin_naive_plus, coverage_Ours_G_haj_lin_naive_plus 
  #               
  ##########################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########################Ours_G_F_ablation#####################################
  
  
  ##########################ablation study 4######################################
  
  
  
  
  
  return(tibble(Leung, Gao, Gao_F, Gao_L,  Ours_X_haj,   Ours_X_haj_lin,               Ours_X_haj_lin_phi,
                var_Leung, var_Gao, var_Gao_F, var_Gao_L,     var_Ours_X_haj,    var_Ours_X_haj_lin,  var_Ours_X_haj_lin_phi,  
                coverage_Leung, coverage_Gao, coverage_Gao_F, coverage_Gao_L,  coverage_Ours_X_haj,     coverage_Ours_X_haj_lin, coverage_Ours_X_haj_lin_phi, 
                var_Leung_naive, var_Gao_naive, var_Gao_F_naive, var_Gao_L_naive,    var_Ours_X_haj_naive,   var_Ours_X_haj_lin_naive, var_Ours_X_haj_lin_naive_phi,
                coverage_Leung_naive, coverage_Gao_naive, coverage_Gao_F_naive, coverage_Gao_L_naive,  coverage_Ours_X_haj_naive,     coverage_Ours_X_haj_lin_naive, coverage_Ours_X_haj_lin_naive_phi,
                
                # 
                # Leung_plus, Gao_plus, Gao_F_plus, Gao_L_plus,  Ours_X_ht_plus, Ours_G_ht_plus,Ours_X_ht_lin_plus, Ours_G_ht_lin_plus, Ours_X_haj_plus, Ours_G_haj_plus,  Ours_X_haj_lin_plus,   Ours_G_haj_lin_plus, 
                # var_Leung_plus, var_Gao_plus, var_Gao_F_plus, var_Gao_L_plus, var_Ours_X_ht_plus, var_Ours_G_ht_plus,  var_Ours_X_ht_lin_plus, var_Ours_G_ht_lin_plus, var_Ours_X_haj_plus, var_Ours_G_haj_plus, var_Ours_X_haj_lin_plus,  var_Ours_G_haj_lin_plus, 
                # coverage_Leung_plus, coverage_Gao_plus, coverage_Gao_F_plus, coverage_Gao_L_plus,  coverage_Ours_X_ht_plus, coverage_Ours_G_ht_plus,  coverage_Ours_X_ht_lin_plus, coverage_Ours_G_ht_lin_plus, coverage_Ours_X_haj_plus, coverage_Ours_G_haj_plus,coverage_Ours_X_haj_lin_plus, coverage_Ours_G_haj_lin_plus,
                # var_Leung_naive_plus, var_Gao_naive_plus, var_Gao_F_naive_plus, var_Gao_L_naive_plus, var_Ours_X_ht_naive_plus, var_Ours_G_ht_naive_plus,  var_Ours_X_ht_lin_naive_plus, var_Ours_G_ht_lin_naive_plus, var_Ours_X_haj_naive_plus, var_Ours_G_haj_naive_plus, var_Ours_X_haj_lin_naive_plus,  var_Ours_G_haj_lin_naive_plus, 
                # coverage_Leung_naive_plus, coverage_Gao_naive_plus, coverage_Gao_F_naive_plus, coverage_Gao_L_naive_plus,  coverage_Ours_X_ht_naive_plus, coverage_Ours_G_ht_naive_plus,  coverage_Ours_X_ht_lin_naive_plus, coverage_Ours_G_ht_lin_naive_plus, coverage_Ours_X_haj_naive_plus, coverage_Ours_G_haj_naive_plus,coverage_Ours_X_haj_lin_naive_plus, coverage_Ours_G_haj_lin_naive_plus 
                # 
                
                
                
  ))
  
})




# sim_res <- sim_res[apply(sim_res, 1, function(row) all(row >= 0)), ]


index <- 7


# if(var_Gao_F>1){Gao_F <- NULL; var_Gao_F <- NULL; coverage_Gao_F <- NULL}
# sim_res <- sim_res[sim_res$var_Gao_F <= 10, ]


print("tau:"); print(tau)


print("Estimation:"); print((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[1:index]) 
print("oracle sd:"); print((sim_res %>% summarise_all(sd)  %>% as.data.frame() )[1:index]) 
print("practical sd:"); print(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(index+1):(index*2)]) 
print("naive sd:"); print(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(3*index+1):(4*index)]) 
print("practical coverage:"); print((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(2*index+1):(3*index)]) 
print("naive coverage:"); print((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(4*index+1):(5*index)])


# print("Estimation+:"); print((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+1):(5*index+index)]) 
# print("oracle sd+:"); print((sim_res %>% summarise_all(sd)  %>% as.data.frame() )[(5*index+1):(5*index+index)]) 
# print("practical sd+:"); print(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+index+1):(5*index+index*2)]) 
# print("naive sd+:"); print(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+3*index+1):(5*index+4*index)]) 
# print("practical coverage+:"); print((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+2*index+1):(5*index+3*index)]) 
# print("naive coverage+:"); print((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+4*index+1):(5*index+5*index)])






oracle <- c()
oracle_plus <- c()


sim_res_oracle_cover <- map_dfr(1:1, ~{
  # 定义变量名称列表
  variables <- c('Leung', 'Gao', 'Gao_F', 'Gao_L',   'Ours_X_haj',    'Ours_X_haj_lin', 'Ours_X_haj_lin_phi')
  # variables_plus <- c('Leung_plus', 'Gao', 'Gao_F', 'Gao_L',  'Ours_G_haj_plus', 'Ours_X_ht_plus', 'Ours_X_haj_plus', 'Ours_G_ht_lin_plus', 'Ours_G_haj_lin_plus', 'Ours_X_ht_lin_plus', 'Ours_X_haj_lin_plus')
  
  # 循环替换变量名称并计算结果
  for (var in variables) {
    o_coverage <- mean(abs(sim_res[[var]] - tau) <= qnorm(0.975) * sd(sim_res[[var]]) )
    cat("Oracle Coverage for", var, ":", o_coverage, "\n")
    oracle <- c(oracle, o_coverage)
  }
  # for (var in variables_plus) {
  #   o_coverage <- mean(abs(sim_res[[var]] - tau) <= qnorm(0.975) * sd(sim_res[[var]]) )
  #   cat("Oracle Coverage for", var, ":", o_coverage, "\n")
  #   oracle_plus <- c(oracle_plus, o_coverage)
  # }
  
  
  # o_coverage_Leung_plus <- mean(abs(sim_resLeung−tau)<=qnorm(0.975)∗sd(simresLeung) )
# o_coverage_Gao_naive_plus <- mean(abs(sim_resGao−tau)<=qnorm(0.975)∗sd(simresGao) )
# o_coverage_Gao_F_plus <- mean(abs(sim_resGaoF−tau)<=qnorm(0.975)∗sd(simresGao_F) )
# o_coverage_Gao_L_plus <- mean(abs(sim_resGaoL−tau)<=qnorm(0.975)∗sd(simresGao_L) )
# o_coverage_Ours_G_ht_plus <- mean(abs(sim_resOurs_G_ht_plus - tau) <= qnorm(0.975) * sd(sim_resOurs_G_ht_plus) )
# o_coverage_Ours_G_haj_plus <- mean(abs(sim_resOurs_G_haj_plus - tau) <= qnorm(0.975) * sd(sim_resOurs_G_haj_plus) )
# o_coverage_Ours_X_ht_plus <- mean(abs(sim_resOurs_X_ht_plus - tau) <= qnorm(0.975) * sd(sim_resOurs_X_ht_plus) )
# o_coverage_Ours_X_haj_plus <- mean(abs(sim_resOurs_X_haj_plus - tau) <= qnorm(0.975) * sd(sim_resOurs_X_haj_plus) )
# 
# o_coverage_Ours_G_ht_plus_lin <- mean(abs(sim_resOurs_G_ht_plus_lin - tau) <= qnorm(0.975) * sd(sim_resOurs_G_ht_plus_lin) )
# o_coverage_Ours_G_haj_plus_lin <- mean(abs(sim_resOurs_G_haj_plus_lin - tau) <= qnorm(0.975) * sd(sim_resOurs_G_haj_plus_lin) )
# o_coverage_Ours_X_ht_plus_lin <- mean(abs(sim_resOurs_X_ht_plus_lin - tau) <= qnorm(0.975) * sd(sim_resOurs_X_ht_plus_lin) )
# o_coverage_Ours_X_haj_plus_lin <- mean(abs(sim_resOurs_X_haj_plus_lin - tau) <= qnorm(0.975) * sd(sim_resOurs_X_haj_plus_lin) )

# return(tibble(o_coverage_Leung_plus, o_coverage_Gao_naive_plus, o_coverage_Gao_F_plus, o_coverage_Gao_L_plus,  o_coverage_Ours_X_ht_plus, o_coverage_Ours_G_ht_plus,  o_coverage_Ours_X_ht_plus_lin, o_coverage_Ours_G_ht_plus_lin, o_coverage_Ours_X_haj_plus, o_coverage_Ours_G_haj_plus, o_coverage_Ours_X_haj_plus_lin, o_coverage_Ours_G_haj_plus_lin))
return(tibble(oracle, oracle_plus))

})








# 指定要保存的文件路径
# file_path <- "/home/ZhihengZhang/1022new/result_synthetic.txt"
# file_path <- "/home/ZhihengZhang/syn_total_new/result_synthetic.txt"
file_path <- "~/regression_interference_server/syn_total_new/result_synthetic.txt"



# 将数据写入文本文件
#truth
write.table(tau, file = file_path, col.names = FALSE)


#estimation
write.table((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[1:index], file = file_path, col.names = FALSE, append = TRUE)
#oracle sd
write.table((sim_res %>% summarise_all(sd)  %>% as.data.frame() )[1:index], file = file_path, col.names = FALSE, append = TRUE)
#practical sd
write.table(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(index+1):(index*2)], file = file_path, col.names = FALSE, append = TRUE)
#naive sd
write.table(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(3*index+1):(4*index)], file = file_path, col.names = FALSE, append = TRUE)
#oracle coverage
write.table((sim_res_oracle_cover$oracle)  , file = file_path, col.names = FALSE, append = TRUE)
#practical coverage
write.table((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(2*index+1):(3*index)], file = file_path, col.names = FALSE, append = TRUE)
#naive coverage
write.table((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(4*index+1):(5*index)], file = file_path, col.names = FALSE, append = TRUE)




# #estimation
# write.table((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+1):(5*index+index)], file = file_path, col.names = FALSE, append = TRUE)
# #oracle sd
# write.table((sim_res %>% summarise_all(sd)  %>% as.data.frame() )[(5*index+1):(5*index+index)], file = file_path, col.names = FALSE, append = TRUE)
# #practical sd
# write.table(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+index+1):(5*index+index*2)], file = file_path, col.names = FALSE, append = TRUE)
# #naive sd
# write.table(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+3*index+1):(5*index+4*index)], file = file_path, col.names = FALSE, append = TRUE)
# #oracle coverage
# write.table((sim_res_oracle_cover$oracle_plus ), file = file_path, col.names = FALSE, append = TRUE)
# #practical coverage
# write.table((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+2*index+1):(5*index+3*index)], file = file_path, col.names = FALSE, append = TRUE)
# #naive coverage
# write.table((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(5*index+4*index+1):(5*index+5*index)], file = file_path, col.names = FALSE, append = TRUE)






###########################################################################################################################################################################################################
# 提示数据已成功写入文件
cat("数据已成功写入文本文件(bn = 3):", file_path, "\n")
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
###########################################################################################################################################################################################################
file_path <- "~/regression_interference_server/syn_total_new/linear_parameter_X_counter.txt"
write.table(X, file = file_path, col.names = FALSE)
file_path <- "~/regression_interference_server/syn_total_new/linear_parameter_Y1_counter.txt"
write.table(Y_1, file = file_path, col.names = FALSE)
file_path <- "~/regression_interference_server/syn_total_new/linear_parameter_Y0_counter.txt"
write.table(Y_0, file = file_path, col.names = FALSE)
file_path <- "~/regression_interference_server/syn_total_new/linear_parameter_noise_counter.txt"
write.table(errors, file = file_path, col.names = FALSE)
file_path <- "~/regression_interference_server/syn_total_new/linear_parameter_ps1_counter.txt"
write.table(pscore1, file = file_path, col.names = FALSE)
file_path <- "~/regression_interference_server/syn_total_new/linear_parameter_ps0_counter.txt"
write.table(pscore0, file = file_path, col.names = FALSE)




tempt <- mean(pscore0 * pscore1 * (Y_1/pscore1 - mean(Y_1)/pscore1 + Y_0/pscore0 - mean(Y_0)/pscore0 )^2)


sigma_haj <- sqrt(tempt / n)


tempt <- mean(pscore0 * pscore1 * (Y_1/pscore1 - 0/pscore1 + Y_0/pscore0 - 0/pscore0 )^2)


sigma_ht <- sqrt(tempt / n)


beta_L_1 <- sum(X * X)^{-1} * sum(X * Y_1)
beta_L_0 <- sum(X * X)^{-1} * sum(X * Y_0)
beta_F <- (beta_L_0+beta_L_1)/2


tempt <- mean(pscore0 * pscore1 * (Y_1/pscore1 - mean(Y_1)/pscore1 + Y_0/pscore0 - mean(Y_0)/pscore0 - X * beta_L_1/pscore1 - X * beta_L_0/pscore0 )^2)


sigma_L <- sqrt(tempt / n)


tempt <- mean(pscore0 * pscore1 * (Y_1/pscore1 - mean(Y_1)/pscore1 + Y_0/pscore0 - mean(Y_0)/pscore0 - X * beta_F/(pscore1*pscore0)  )^2)


sigma_F <- sqrt(tempt / n)


tempt1 <- sum(pscore0/pscore1 * X * X)
tempt2 <- sum(X * X)
tempt3 <- tempt1 <- sum(pscore1/pscore0 * X * X)
tempt_left <- matrix(c(tempt1, tempt2, tempt2, tempt3), ncol = 2)


tempt1 <- sum(pscore0/pscore1 * X * Y_1 + X * Y_0)
tempt2 <- sum(pscore1/pscore0 * X * Y_0 + X * Y_1)




tilde_beta_1 <- (solve(tempt_left) %*% (matrix(cbind(tempt1, tempt2))))[1,] 
tilde_beta_0 <- (solve(tempt_left) %*% (matrix(cbind(tempt1, tempt2))))[2,] 


delta <- mean(pscore1 * pscore0 * (X * tilde_beta_1/pscore1 + X * tilde_beta_0/pscore0)^2)


delta_1 <- mean(pscore1 * pscore0 * (X * (tilde_beta_1-beta_L_1)/pscore1 + X * (tilde_beta_0- beta_L_0)/pscore0)^2)


sighaj_sigL <- delta - delta_1


sighaj_sigL / (sigma_haj^2 * n)
sqrt(abs(sighaj_sigL)/n)/sigma_haj 


delta_2 <- mean(pscore1 * pscore0 * (X * (tilde_beta_1-beta_F)/pscore1 + X * (tilde_beta_0- beta_F)/pscore0)^2)


sighaj_sigF <- delta - delta_2


sighaj_sigF / (sigma_haj^2 * n)












