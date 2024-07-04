library(igraph)
library(tidyverse)
library(stringdist)
# library(dplyr)

#to do list:
#re-name and add new x-based general-variable estimator: add Gao_F; Ours_X_ht_plus; Ours_X_haj_plus; 1+3+4, also for var and coverage;
#add oracle and naive coverage: naive means dependence = 0 and estimate the i.i.d standard errors


#first， HT should add a intersection; (its ok; and delete non-necessary scale!)
#second, should report the iteraction result; and revise our presentation! (ok!)
#third, add the oracle average and naive std;
#fourth, should claim G is better than X!


n <- 800
r1 <- 0.5 

ball_vol <- function(d,r){
  volume <- pi^(d/2) * r^d / gamma(d/2+1)
  return(volume)
}

positions <- matrix(runif(2 * n), nrow = n, ncol = 2)
# Calculate distance matrix
dist_matrix <- as.matrix(dist(positions, method = "euclidean"))
r <- (1 / ball_vol(2, 1) / n)^{1/2}  # RGG parameter calculation
# Create adjacency matrix based on distances and threshold r
E <- dist_matrix <= r
E <- (E) * 1


print(E)
# Fill diagonal with zeros to avoid self-loops

# #尝试性改动：
# for (i in 1: ncol(E)){
#   E[i,i] <- 0
# }


# 计算矩阵的行和
row_sums <- rowSums(E)
# 对非负整数矩阵进行行和归一化











num_nb <- rowSums(E)
# transform the objective
g <- graph_from_adjacency_matrix(as.matrix(E), mode = "undirected")
# compute the average path length
avg_path_length <- mean_distance(g)
G <- E/rowSums(E)
# Compute errors with random noise
errors <- rnorm(n) + (positions[,1] - 0.5)
#we need to make sure the degree is at least 1, naturally
#To avoid the mistake:
rows_all_zero <- which(apply(G, 1, function(row) all(row == 0)))
cols_all_zero <- which(apply(G, 2, function(col) all(col == 0)))
print("全部都是零元素的行索引:")
print(rows_all_zero)
print("全部都是零元素的列索引:")
print(cols_all_zero)
# G <- G[-rows_all_zero, -cols_all_zero]
# if (length(rows_all_zero) > 0) {
#   G <- G[-rows_all_zero, ]
# }
# if (length(cols_all_zero) > 0) {
#   G <- G[, -cols_all_zero]
# }
print(G)

#we need to use a new generation: linear-in-means and non-linear model;
X <-rnorm(n) %>% scale(scale = FALSE); epsilon <- errors #the normalized n-dimensional vector corresponding to each node to generate $X$ and $\epsilon$.

#now it is to generate the linear-in-means model:
get_Y <- function(Z){
  return(solve(-0.8*G+diag(n),-rep(1,times=n)+G%*%Z+Z+X+epsilon) %>% drop())
}
#we also define another non-linear generation process:
get_Y_nonlinear <- function(Z){
  Y_0 <- (-1 + 1* G%*%Z + Z + X + errors) > 0
  Y_next <- Y_0 + 1
  while( max( abs(Y_0 - Y_next) ) > 0.0001){
    Y_0 <- Y_next
    Y_next <- (-1 + 1.5* G%*%Y_0  + 1* G%*%Z + Z + X + errors) >0
  }
  return(Y_0)
}

get_T <- function(Z){
  return(drop(E%*%Z > 0) *1)
}

pscore0 <- pbinom(0 ,size = num_nb, prob = r1); pscore1 <- 1-pscore0


A <- ((E %*% E)>0)*1; temp <- eigen(A);  A_p <- (temp$vectors)%*%diag((temp$values)*(temp$values>0))%*%solve(temp$vectors) 
#it is to generate the new A_p; here we choose $b_n = 3$, or $b_n = 2$;

get_X <- function(X,Z,G){
  return(matrix(c(Z,drop(G%*%Z),X,drop(G%*%X)), nrow=n, ncol = 4))      #it is a generalized variable constructed by (Z, G*Z, X, G*X);           
}


#we aim to calculate the coefficient:
mom_mat <- matrix(0, nrow = n, ncol = 5)
for(i in 1:1000){
  Z <- rbinom(n, size = 1, prob = r1);  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
  T_vec <- get_T(Z); w <- T_vec/pscore1-(1-T_vec)/pscore0 #They are both $n*1$ vectors;
  mom_mat <- mom_mat + c(w^2, X_aug*w) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
}
orth_coef <- mom_mat[, 2:5] / mom_mat[, 1]


mom_mat <- matrix(0, nrow = n, ncol = 5)
for(i in 1:1000){
  Z <- rbinom(n, size = 1, prob = r1);  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
  T_vec <- get_T(Z); w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0)) #They are both $n*1$ vectors;
  mom_mat <- mom_mat + c(w_haj^2, X_aug*w_haj) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
}
orth_coef_haj <- mom_mat[, 2:5] / mom_mat[, 1]

#we also need to compute the iteraction debiasing procedure:
mom_mat <- matrix(0, nrow = n, ncol = 5+4)
for(i in 1:1000){
  Z <- rbinom(n, size = 1, prob = r1);  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
  T_vec <- get_T(Z); w <- T_vec/pscore1-(1-T_vec)/pscore0 #They are both $n*1$ vectors;
  X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
  mom_mat <- mom_mat + c(w^2, X_aug_lin*w) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
}
orth_coef_lin <- mom_mat[, 2:(5+4)] / mom_mat[, 1]

mom_mat <- matrix(0, nrow = n, ncol = 5+4)
for(i in 1:1000){
  Z <- rbinom(n, size = 1, prob = r1);  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
  T_vec <- get_T(Z); w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0)) #They are both $n*1$ vectors;
  X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
  mom_mat <- mom_mat + c(w_haj^2, X_aug_lin*w_haj) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
}
orth_coef_haj_lin <- mom_mat[, 2:(5+4)] / mom_mat[, 1]









paste("orth_coef:", orth_coef)

#we compute the ground truth:
tau <- map_dbl(1:1000, ~{
  Z <- rbinom(n, size = 1, prob = r1); Y <- get_Y(Z)
  T_vec <- get_T(Z); D <- Y*T_vec/pscore1-Y*(1-T_vec)/pscore0
  return(mean(D))
}) %>% mean()

tau_haj <- map_dbl(1:1000, ~{
  Z <- rbinom(n, size = 1, prob = r1); Y <- get_Y(Z)
  T_vec <- get_T(Z); D <- Y*T_vec/(pscore1*mean(T_vec/pscore1))-Y*(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  # w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  return(mean(D))
}) %>% mean()

sum <- 0

sim_res<- map_dfr(1:100, ~{
  sum <- (sum+1); print("sum:"); print(sum)
  Z <- rbinom(n, size = 1, prob = r1); Y <- get_Y(Z); X_aug <- get_X(X,Z,G)
  T_vec <- get_T(Z)
  w <- T_vec/pscore1-(1-T_vec)/pscore0
  w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  #add definition:
  w_1 <- T_vec/pscore1
  w_0 <- (1-T_vec)/pscore0
  w_haj_1 <- T_vec/(pscore1*mean(T_vec/pscore1))
  w_haj_0 <- (1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  
  
  D <- Y*w
  Leung <- mean(D); 
  D_haj <- Y*w_haj
  # Gao_naive <- mean(D_haj)
  
  
  #Here is the un-adjusted estimator:
  var_Leung <- t(D-Leung)%*%A%*%(D-Leung)/n^2 %>% as.vector();
  var_Leung_plus <- t(D-Leung)%*%A_p%*%(D-Leung)/n^2 %>% as.vector()
  # is_cover_unadj_1 <- abs(Leung-tau)<=qnorm(0.975)*sqrt(var_est_unadj_1)
  coverage_Leung <- abs(Leung-tau)<=qnorm(0.975)*sqrt(var_Leung)
  
  
  
  #We start to construct our general auxiliary methods:
  X_db <- X_aug - (w) * (orth_coef)  #it is n*4;  n*1,  n*4
  X_db <- cbind(X_db, 1)
  # X_db <- cbind(X_db)
  # D_2 <- scale(X_db*w, scale = FALSE)
  D_2 <- X_db * w
  #拓展到interaction时候优化要重新处理
  hbeta_2 <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-Leung)) #here we should use the new variance estimator:
  Ours_G_ht_plus <- mean((Y-X_db%*%hbeta_2)*w) 
  var_Ours_G_ht_plus <- t(D-D_2%*%hbeta_2-Ours_G_ht_plus)%*%A_p%*%(D-D_2%*%hbeta_2-Ours_G_ht_plus)/n^2 %>% as.vector()
  # coverage_Ours_G_ht <- abs(Ours_G_ht-tau)<=qnorm(0.975)*sqrt(var_Ours_G_ht)
  coverage_Ours_G_ht_plus <- abs(Ours_G_ht_plus - tau)<=qnorm(0.975)*sqrt(var_Ours_G_ht_plus)
  
  
  
  X_db <- X_aug - w_haj * (orth_coef_haj)  #it is n*4;  n*1,  n*4
  # D_2 <- scale(X_db*w, scale = FALSE) 
  D_2 <- X_db * w
  hbeta_2_haj <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
  Ours_G_haj_plus <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  # var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
  var_Ours_G_haj_plus <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  coverage_Ours_G_haj_plus <- abs(Ours_G_haj_plus - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_plus)
  
  
  
  #We also consider G == X:
  X_db <- X  #it is n*4;  n*1,  n*4
  X_db <- cbind(X_db, 1)
  # D_2 <- scale(X_db*w, scale = FALSE)
  D_2 <- X_db * w
  hbeta_2 <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-Leung)) #here we should use the new variance estimator:
  Ours_X_ht_plus <- mean((Y-X_db%*%hbeta_2)*w) 
  #I have done here: compute the var and coverage:
  # var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
  var_Ours_X_ht_plus <- t(D-D_2%*%hbeta_2-Ours_X_ht_plus)%*%A_p%*%(D-D_2%*%hbeta_2-Ours_X_ht_plus)/n^2 %>% as.vector()
  coverage_Ours_X_ht_plus <- abs(Ours_X_ht_plus - tau)<=qnorm(0.975)*sqrt(var_Ours_X_ht_plus)
  
  X_db <- X  #it is n*4;  n*1,  n*4
  D_2 <- X_db * w
  hbeta_2_haj <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
  Ours_X_haj_plus <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  # var_Ours_X_ht_plus <- t(D-D_2%*%hbeta_2-Leung)%*%A_p%*%(D-D_2%*%hbeta_2-Leung)/n^2 %>% as.vector()
  var_Ours_X_haj_plus <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # coverage_Ours_G_ht <- abs(Ours_G_ht-tau)<=qnorm(0.975)*sqrt(var_Ours_G_ht)
  coverage_Ours_X_haj_plus <- abs(Ours_X_haj_plus - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_plus)
  
  
  
  ##############################################################################################################
  #Moreover, we use the lin's method!  G+HT+lin; Lin的第一种
  X_db <- cbind(X_aug * T_vec, X_aug * (1-T_vec)) #这是初始的G
  X_db <- X_db - w * (orth_coef_lin)  #it is n*4;  n*1,  n*4
  # X_db <- cbind(X_db,2) #5个维度
  X_db <- X_db[, -(5:6)]
  X_db <- cbind(X_db,1)
  # X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  D_2 <- X_db * w
  # D <- Y*w
  #拓展到interaction时候优化要重新处理
  quadratic_function <- function(hbeta) {
    result <- t(D- D_2%*%hbeta  -Leung) %*% A_p %*% (D- D_2%*%hbeta  - Leung )/n^2 %>% as.vector()
    result<- sqrt(result)
    return(result)
  }
  # 使用优化函数找到最小值和对应的位置
  result <- optim( vector("logical", length = 2*4-2 +1   ) * 1,          quadratic_function)
  # 输出最优值和最优位置
  cat("最小值：", result$value, "\n")
  cat("最优位置：", result$par, "\n")
  hbeta_2_ht <- result$par
  Ours_G_ht_plus_lin <- mean((Y-X_db%*%hbeta_2_ht)*w)
  var_Ours_G_ht_plus_lin <- t(D- D_2%*%hbeta_2_ht  - Ours_G_ht_plus_lin  ) %*%A_p %*% (D- D_2%*%hbeta_2_ht  - Ours_G_ht_plus_lin )/n^2 %>% as.vector()
  # var_Ours_G_ht_plus_lin <- result$value
  
  
  # hbeta_2 <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-Leung)) #here we should use the new variance estimator:
  # Ours_G_ht_plus <- mean((Y-X_db%*%hbeta_2)*w)
  # var_Ours_G_ht_plus <- t(D-D_2%*%hbeta_2-Ours_G_ht_plus)%*%A_p%*%(D-D_2%*%hbeta_2-Ours_G_ht_plus)/n^2 %>% as.vector()
  # coverage_Ours_G_ht <- abs(Ours_G_ht-tau)<=qnorm(0.975)*sqrt(var_Ours_G_ht)
  # var_Ours_G_ht_plus_lin <- var_Ours_G_ht_plus_lin * var_Ours_G_ht_plus_lin  #for esay computing
  coverage_Ours_G_ht_plus_lin <- abs(Ours_G_ht_plus_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_G_ht_plus_lin)
  # #重新尝试：
  # X_db <- X_aug - (w) * (orth_coef) #it is n*4;  n*1,  n*4
  # # X_db <- cbind(X_db, 1)
  # # D_2 <- scale(X_db*w, scale = FALSE) 
  # X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  # D <- Y * w
  # D_2 <- X_db * w
  # 
  # # D_2 <- scale(X_db * w)
  # # D_2 <- scale(X_db*w, scale = FALSE) #在HT里，做归一化是允许的， 
  # hbeta_2_ht <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-Leung    )) #here we should use the new variance estimator:
  # # Ours_G_ht_plus_lin <- mean((Y-X_db%*%hbeta_2_ht)*w)
  # Ours_G_ht_plus_lin <- mean(D- D_2%*%hbeta_2_ht)
  # # var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
  # var_Ours_G_ht_plus_lin <- t(D- D_2%*%hbeta_2_ht  - Ours_G_ht_plus_lin  ) %*%A_p %*% (D- D_2%*%hbeta_2_ht  - Ours_G_ht_plus_lin )/n^2 %>% as.vector()
  # # coverage_Ours_G_ht_plus_lin <- abs(Ours_G_ht_plus_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_G_ht_plus_lin)
  # # var_Ours_G_haj_plus_lin <-  var_Ours_G_haj_plus_lin *  var_Ours_G_haj_plus_lin #为了方便计算
  
  
  
  
  
  #Moreover, we use the lin's method!  G+Haj+lin； Lin的第二种
  X_db <- cbind(X_aug * T_vec, X_aug * (1-T_vec)) #这是初始的G
  X_db <- X_db - w_haj * (orth_coef_haj_lin)  #it is n*4;  n*1,  n*4
  # X_db <- X_aug  #it is n*4;  n*1,  n*4
  X_db <- X_db[, -(5:6)]
  # D_2 <- scale(X_db*w, scale = FALSE) 
  # X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  D_2 <- X_db * w
  D <- Y * w
  # D_2_1 <- D_2[T_vec == 1, 1:4]; D_2_0 <- D_2[T_vec == 0, 5:8]
  # D1 <- D[T_vec == 1]; D0 <- D[T_vec == 0 ]
  # hbeta_2_haj_1 <- solve(t(D_2_1)%*%A_p%*%(D_2_1),   t(D_2_1)%*%A_p%*%(D1- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) 
  # hbeta_2_haj_0 <- solve(t(D_2_0)%*%A_p%*%(D_2_0),   t(D_2_0)%*%A_p%*%(D0- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ))
  quadratic_function <- function(hbeta) {
    result <- t(D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*% A_p %*% (D- D_2%*%hbeta  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)  )/n^2 %>% as.vector()
    result<- sqrt(result)
    return(result)
  }
  # 使用优化函数找到最小值和对应的位置
  result <- optim( vector("logical", length = 2*4-2    ) * 1,          quadratic_function)
  # 输出最优值和最优位置
  cat("最小值：", result$value, "\n")
  cat("最优位置：", result$par, "\n")
  hbeta_2_haj <- result$par
  Ours_G_haj_plus_lin <- mean((Y-X_db%*%hbeta_2_haj)*w)
  
  
  # hbeta_2_haj <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
  Ours_G_haj_plus_lin <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  # var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
  var_Ours_G_haj_plus_lin <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  
  coverage_Ours_G_haj_plus_lin <- abs(Ours_G_haj_plus_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_plus_lin)
  # var_Ours_G_haj_plus_lin <-  var_Ours_G_haj_plus_lin *  var_Ours_G_haj_plus_lin #为了方便计算
  
  
  
  
  
  #Moreover, we use the lin's method!  X+HT+lin； Lin的第三种
  # X_db <- X  #it is n*4;  n*1,  n*4
  # X_db <- cbind(X_db,1) #5个维度
  # X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  # D_2 <- X_db * w
  # # D <- Y * w
  # #拓展到interaction时候优化要重新处理
  # quadratic_function <- function(hbeta) {
  #   result <- t(D- D_2%*%hbeta  -Leung ) %*% A_p %*% (D- D_2%*%hbeta  -Leung )/n^2 %>% as.vector()
  #   result<- sqrt(result)
  #   return(result)
  # }
  # # 使用优化函数找到最小值和对应的位置
  # result <- optim( vector("logical", length = 2*2    ) * 1,          quadratic_function)
  # # 输出最优值和最优位置
  # cat("最小值：", result$value, "\n")
  # cat("最优位置：", result$par, "\n")
  # hbeta_2_ht <- result$par
  # var_Ours_X_ht_plus_lin <- result$value
  # var_Ours_X_ht_plus_lin <- var_Ours_X_ht_plus_lin * var_Ours_X_ht_plus_lin #easy for computing;
  # Ours_X_ht_plus_lin <- mean((Y-X_db%*%hbeta_2_ht)*w)
  # 
  # hbeta_2_ht <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-Leung)) #here we should use the new variance estimator:
  # Ours_X_ht_plus_lin <- mean((Y-X_db%*%hbeta_2_ht)*w)
  # var_Ours_X_ht_plus_lin <- t(D-D_2%*%hbeta_2_ht-Leung)%*%A_p%*%(D-D_2%*%hbeta_2_ht-Leung)/n^2 %>% as.vector()
  # # coverage_Ours_G_ht <- abs(Ours_G_ht-tau)<=qnorm(0.975)*sqrt(var_Ours_G_ht)
  # coverage_Ours_X_ht_plus_lin <- abs(Ours_X_ht_plus_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_X_ht_plus_lin)
  #Moreover, we use the lin's method!  X+Haj+lin/有问题，完全仿照最后对的试一下
  X_db <- X #it is n*4;  n*1,  n*4
  # X_db <- cbind(X_db, 1)
  # D_2 <- scale(X_db*w, scale = FALSE) 
  X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  X_db <- cbind(X_db, 1)
  D_2 <- X_db * w
  # D_2 <- scale(X_db*w, scale = FALSE) 
  hbeta_2_ht <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-Leung    )) #here we should use the new variance estimator:
  Ours_X_ht_plus_lin <- mean((Y-X_db%*%hbeta_2_ht)*w)
  # var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
  var_Ours_X_ht_plus_lin <- t(D- D_2%*%hbeta_2_ht  - Ours_X_ht_plus_lin  ) %*%A_p %*% (D- D_2%*%hbeta_2_ht  - Ours_X_ht_plus_lin )/n^2 %>% as.vector()
  coverage_Ours_X_ht_plus_lin <- abs(Ours_X_ht_plus_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_X_ht_plus_lin)
  # var_Ours_G_haj_plus_lin <-  var_Ours_G_haj_plus_lin *  var_Ours_G_haj_plus_lin #为了方便计算
  
  
  
  
  
  
  
  
  #Moreover, we use the lin's method!  X+Haj+lin；lin的第四种
  X_db <- X #it is n*4;  n*1,  n*4
  # D_2 <- scale(X_db*w, scale = FALSE) 
  X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  D_2 <- X_db * w
  hbeta_2_haj <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
  Ours_X_haj_plus_lin <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  # var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
  var_Ours_X_haj_plus_lin <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  coverage_Ours_X_haj_plus_lin <- abs(Ours_X_haj_plus_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_plus_lin)
  # var_Ours_G_haj_plus_lin <-  var_Ours_G_haj_plus_lin *  var_Ours_G_haj_plus_lin #为了方便计算
  
  
  
  
  
  
  
  
  
  
  
  lm_haj <- lm(Y~1+T_vec+T_vec:X, w = T_vec/pscore1+(1-T_vec)/pscore0)
  w <- T_vec/pscore1+(1-T_vec)/pscore0
  e_haj <- lm_haj %>% resid(); Gao_L <- lm_haj %>% coef() %>% .[2]
  C_haj <- cbind(1,T_vec, T_vec*X)
  var_Gao_L <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  var_Gao_L_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  #最好用我们的方案
  coverage_Gao_L <- abs(Gao_L-tau)<=qnorm(0.975)*sqrt(var_Gao_L)
  
  
  lm_haj <- lm(Y~1+T_vec+X, w = T_vec/pscore1+(1-T_vec)/pscore0)
  w <- T_vec/pscore1+(1-T_vec)/pscore0
  e_haj <- lm_haj %>% resid(); Gao_F <- lm_haj %>% coef() %>% .[2]
  C_haj <- cbind(1,T_vec,X)
  var_Gao_F <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  var_Gao_F_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  coverage_Gao_F_plus <- abs(Gao_F-tau)<=qnorm(0.975)*sqrt(var_Gao_F_plus)
  
  
  lm_haj <- lm(Y~1+T_vec, w = T_vec/pscore1+(1-T_vec)/pscore0)
  w <- T_vec/pscore1+(1-T_vec)/pscore0
  e_haj <- lm_haj %>% resid(); Gao_naive <- lm_haj %>% coef() %>% .[2]
  C_haj <- cbind(1,T_vec)
  var_Gao_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  var_Gao_naive_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  coverage_Gao_naive_plus <- abs(Gao_naive-tau)<=qnorm(0.975)*sqrt(var_Gao_naive_plus)
  
  
  
  
  
  
  
  
  
  return(tibble(Leung, Gao_naive, Gao_F, Gao_L,  Ours_G_ht_plus, Ours_G_haj_plus, Ours_X_ht_plus, Ours_X_haj_plus, Ours_G_ht_plus_lin, Ours_G_haj_plus_lin, Ours_X_ht_plus_lin, Ours_X_haj_plus_lin, 
                var_Leung_plus, var_Gao_naive_plus, var_Gao_F_plus, var_Gao_L_plus, var_Ours_G_ht_plus, var_Ours_G_haj_plus, var_Ours_X_ht_plus, var_Ours_X_haj_plus, var_Ours_G_ht_plus_lin, var_Ours_G_haj_plus_lin, var_Ours_X_ht_plus_lin, var_Ours_X_haj_plus_lin, 
                coverage_Leung, coverage_Gao_naive_plus, coverage_Gao_F_plus, coverage_Gao_L, coverage_Ours_G_ht_plus, coverage_Ours_G_haj_plus, coverage_Ours_X_ht_plus, coverage_Ours_X_haj_plus, coverage_Ours_G_ht_plus_lin, coverage_Ours_G_haj_plus_lin, coverage_Ours_X_ht_plus_lin, coverage_Ours_X_haj_plus_lin  ))
})





# if(var_Gao_F>1){Gao_F <- NULL; var_Gao_F <- NULL; coverage_Gao_F <- NULL}




# sim_res <- sim_res[sim_res$var_Gao_F <= 10, ]






print("tau:"); print(tau)
# sim_res %>% summarise_all(mean)  %>% as.data.frame()
print("Estimation:"); print((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[1:12]) 
# sim_res %>% summarise_all(sd)  %>% as.data.frame()
print("practical sd:"); print(sqrt(sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(12+1):(12*2)]) 
print("practical coverage:"); print((sim_res %>% summarise_all(mean)  %>% as.data.frame() )[(2*12+1):(3*12)]) 
print("oracle sd:"); print((sim_res %>% summarise_all(sd)  %>% as.data.frame() )[1:12]) 


# warnings()




# Gao_naive <- mean(Y*T_vec/pscore1)/mean(T_vec/pscore1)-mean(Y*(1-T_vec)/pscore0)/mean((1-T_vec)/pscore0)
