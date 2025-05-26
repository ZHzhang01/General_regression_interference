library(igraph)
library(tidyverse)
library(stringdist)
# library(dplyr)
# 安装并加载caret包
#library(caret)




n <- 3000
r1 <- 0.5 

ball_vol <- function(d,r){
  volume <- pi^(d/2) * r^d / gamma(d/2+1)
  return(volume)
}

positions <- matrix(runif(2 * n), nrow = n, ncol = 2)
# Calculate distance matrix
dist_matrix <- as.matrix(dist(positions, method = "euclidean"))
r <- (1.5 / (ball_vol(2, 1) * n))^{1/2}  # RGG parameter calculation， 
# Create adjacency matrix based on distances and threshold r
E <- (dist_matrix <= r)
E <- (E) * 1
for (i in 1:nrow(E)){
  E[i,i] <- 0
}

g <- graph_from_adjacency_matrix(as.matrix(E), mode = "undirected")
# compute the average path length
avg_path_length <- mean_distance(g)


#we need to make sure the degree is at least 1, naturally
#To avoid the mistake:
# rows_all_zero <- which(apply(E, 1, function(row) all(row == 0)))
# cols_all_zero <- which(apply(E, 2, function(col) all(col == 0)))
# # print("全部都是零元素的行索引:")
# # print(rows_all_zero)
# # print("全部都是零元素的列索引:")
# # print(cols_all_zero)
# E <- E[-rows_all_zero, ]
# E <- E[, -cols_all_zero]






# 寻找所有节点相连的边
connected_edges <- which(E != 0, arr.ind = TRUE)

# 获取所有节点
nodes <- unique(c(connected_edges[, 1], connected_edges[, 2]))

# 创建新的对称邻接矩阵E_new
E_new <- matrix(0, nrow = length(nodes), ncol = length(nodes))

# 更新邻接矩阵
for (i in 1:nrow(connected_edges)) {
  row <- connected_edges[i, 1]
  col <- connected_edges[i, 2]
  E_new[match(row, nodes), match(col, nodes)] <- 1
  E_new[match(col, nodes), match(row, nodes)] <- 1
}



print("删除没有节点相连的边后的对称邻接矩阵E_new：")
print(E_new)

E <- E_new





# positions <- positions[-rows_all_zero, ]
positions <- matrix(runif(2 * ncol(E)), nrow = ncol(E), ncol = 2)
# positions <- positions[-rows_all_zero, ]
# if (length(rows_all_zero) > 0) {
#   E <- E[-rows_all_zero, ]
# }
# if (length(cols_all_zero) > 0) {
#   E <- E[, -cols_all_zero]
# }
# print(E)
num_nb <- rowSums(E)
G <- E/rowSums(E)
n <- ncol(G)

#re-design the G:#############################################################################################################
# E <- sample_degseq(
#   out.deg = ceiling(runif(n,min = 0, max = 4)), method = c("simple.no.multiple")
# ) %>%  as_adjacency_matrix()  %>% as.matrix() 
# num_nb <- rowSums(E)
# G <- E/rowSums(E)
# n <- ncol(G)
#############################################################################################################################

# file_path <- "/home/ZhihengZhang/syn_total_new/5000_linear_bn_3.txt"
# Compute errors with random noise
errors <- rnorm(n) + (positions[,1] - 0.5)
X <-rnorm(n) %>% scale(scale = FALSE); epsilon <- errors

#目前效果很好：
# file_path <- "linear_parameter_X.txt"
# write.table(X, file = file_path, col.names = FALSE)
# file_path <- "linear_parameter_G.txt"
# write.table(G, file = file_path, col.names = FALSE)
# file_path <- "linear_parameter_noise.txt"
# write.table(errors, file = file_path, col.names = FALSE)






# 
data1 <- read.table("/home/ZhihengZhang/Final_response/syn/linear_parameter_X.txt", header = FALSE)
data2 <- read.table("/home/ZhihengZhang/Final_response/syn/linear_parameter_G.txt", header = FALSE)
data3 <- read.table("/home/ZhihengZhang/Final_response/syn/linear_parameter_noise.txt", header = FALSE)
# 
#data1 <- read.table("~/regression_interference_server/previous_data/syn_total_new/linear_parameter_X.txt", header = FALSE)
#data2 <- read.table("~/regression_interference_server/previous_data/syn_total_new/linear_parameter_G.txt", header = FALSE)
#data3 <- read.table("~/regression_interference_server/previous_data/syn_total_new/linear_parameter_noise.txt", header = FALSE)
# ~/regression_interference_server/previous_data/syn_total_new
#
#
#
#
#
#extract data from table instead!!!:
# 
# data1 <- read.table("~/linear_parameter_X.txt", header = FALSE)
# data2 <- read.table("~/linear_parameter_G.txt", header = FALSE)
# data3 <- read.table("~/linear_parameter_noise.txt", header = FALSE)

# 
# 
X <- as.matrix(data1[, -1])  # 提取向量
G <- as.matrix(data2[, -1])
E <- apply(G, c(1, 2), function(x) ifelse(x != 0, 1, 0))
num_nb <- rowSums(E)
errors <- as.vector(data3[, -1])   # 提取向量




############################



n <- length(X)
r1 <- 0.5
# num_nb <- apply(G, 1, function(x) sum(x != 0))


#now it is to generate the linear-in-means model:
# get_Y <- function(Z){
#   return(solve(-0.8*G+diag(n),-rep(1,times=n)+G%*%Z+Z+X+epsilon) %>% drop())
# }




# mat1 <- matrix(0, length(X), length(X))  # Create an N x N matrix filled with zeros
# diag(mat1) <- 2*runif( length(X) )  # Set the diagonal elements to random real numbers between 0 and 1
# 
# mat2 <- matrix(0, length(X), length(X))  # Create an N x N matrix filled with zeros
# diag(mat2) <- 2*runif( length(X) )  # Set the diagonal elements to random real numbers between 0 and 1
# 
# mat3 <- matrix(0, length(X), length(X))  # Create an N x N matrix filled with zeros
# diag(mat3) <- 2*runif( length(X) )  # Set the diagonal elements to random real numbers between 0 and 1
# 
# mat4 <- matrix(0, length(X), length(X))  # Create an N x N matrix filled with zeros
# diag(mat4) <- 2*runif( length(X) )  # Set the diagonal elements to random real numbers between 0 and 1


# Z <- rbinom(n, size = 1, prob = r1);  
G <- as.matrix(G)

get_Y <- function(Z){
  # alpha2 <- ifelse(G%*%Z>0.5, 2, 0.5)
  # alpha3 <- ifelse(G%*%Z>0.5, 2, 0.5)
  # alpha4 <- ifelse(G%*%Z>0.5, 2, -2)
  # alpha5 <- ifelse(G%*%Z>0.5, 2, 0.5)
  # alpha1 <- ifelse(G%*%Z>0.5, 0.1, 0.1)
  
  alpha1 <- 0.1
  alpha2 <- 1
  alpha3 <- 1
  alpha4 <- 1
  alpha5 <- 1
  
  Y_0 <- (-1 + alpha2 * G%*%Z +  alpha3 * Z + alpha4 * X + alpha5 * errors); Y_0 <- as.numeric(Y_0)
  # Y_0[Y_0 > 100] <- 100
  
  Y_next <- (-1 +  alpha1 * (G%*% (Y_0))  +  alpha2 * G%*%Z +  alpha3 * Z + alpha4 * X + alpha5 * errors); Y_next <- as.numeric(Y_next)
  
  while( max( abs(as.numeric(Y_0) - as.numeric(Y_next)) ) > 0.0001){
    Y_0 <- Y_next; Y_0 <- as.numeric(Y_0)
    Y_next <- (-1 +  alpha1 * (G%*%Y_0)  +  alpha2 * G%*%Z +  alpha3 * Z + alpha4 * X + alpha5 * errors); Y_next <- as.numeric(Y_next)
  }
  return(Y_0)
}

get_Y_heter <- function(Z){
  # Y_0 <- (-1 + G%*%Z + Z + X + errors)
  # Y_next <- (-1 + mat1 %*% G%*%Y_0  +  mat2 %*% G%*%Z + mat3 %*% Z + mat4 %*% X + errors)
  # while( max( abs(Y_0 - Y_next) ) > 0.0001){
  #   Y_0 <- Y_next
  #   
  #   # para_1 <- ifelse( G%*%Y_0 < 0.5, 1, 5)
  #   # para_2 <- ifelse( G%*%Z < 0.5, 1,     )
  #   Y_next <- (-1 + mat1 %*% G%*%Y_0  +  mat2 %*% G%*%Z + mat3 %*% Z + mat4 %*% X + errors)
  # }
  
  #rewrite:
  term1 <- solve(diag(length(X)) - mat1 %*% G) 
  term2 <-  (-diag(length(X)) %*% rep(1, length(X)) + (mat2 %*% G + mat3) %*% Z + mat4 %*% X + errors)
  Y <- term1 %*% term2 
  
  return(Y)
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

# threshold <- floor(num_nb * 0.5)
threshold <- 0.5 * num_nb

get_T <- function(Z){
  return(drop(  floor(E %*% Z) > threshold ) *1)
}

# pscore0 <- pbinom(0 ,size = num_nb, prob = r1); pscore1 <- 1-pscore0
pscore0 <- pbinom(threshold ,size = num_nb, prob = r1); pscore1 <- 1-pscore0 #we add a new test!

# 创建图对象
g <- graph_from_adjacency_matrix(E, mode = "undirected")
# 计算最短路径长度矩阵
shortest_paths_mat <- shortest.paths(g, mode = "all")
# 输出最短路径长度矩阵
# print(shortest_paths_mat)


if (avg_path_length < 2*log(n)/log(sum(E)/n)){
  bn <- avg_path_length/2
}
if (avg_path_length >= 2*log(n)/log(sum(E)/n)){
  bn <- avg_path_length^{1/3}
}

K<-1
bn<- round(max(bn, 2*K)) 
# bn <- 2
# bn <- 2
bn <- 3
A <- (shortest_paths_mat <= bn); temp <- eigen(A);  A_p <- (temp$vectors)%*%diag((temp$values)*(temp$values >= 0))%*%solve(temp$vectors) 
#it is to generate the new A; here we choose $b_n = 3$, or $b_n = 2$;

get_X <- function(X,Z,G){
  return(matrix(c(Z,drop(G%*%Z),X,drop(G%*%X)), nrow=n, ncol = 4))      #it is a generalized variable constructed by (Z, G*Z, X, G*X);
}

# get_X <- function(X,Z,G){
#   return(matrix(c(Z,drop(G%*%Z),X,1), nrow=n, ncol = 4))      #it is a generalized variable constructed by (Z, G*Z, X, G*X);           
# }

pscore0 <- as.numeric(pscore0)
pscore1 <- as.numeric(pscore1)


#we aim to calculate the coefficient:
mom_mat <- matrix(0, nrow = n, ncol = 5)
mom_mat_haj <- matrix(0, nrow = n, ncol = 5)
mom_mat_lin <- matrix(0, nrow = n, ncol = 5+4)
mom_mat_haj_lin <- matrix(0, nrow = n, ncol = 5+4)
for(i in 1:10000){
  Z <- rbinom(n, size = 1, prob = r1);  
  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
  T_vec <- get_T(Z);
  X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
  
  w <- T_vec/pscore1-(1-T_vec)/pscore0 #They are both $n*1$ vectors;
  w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  mom_mat <- mom_mat + c(w^2, X_aug*w) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
  mom_mat_haj <- mom_mat_haj + c(w_haj^2, X_aug*w_haj)
  mom_mat_lin <- mom_mat_lin + c(w^2, X_aug_lin*w) 
  mom_mat_haj_lin <- mom_mat_haj_lin + c(w_haj^2, X_aug_lin*w_haj) 
}
orth_coef <- mom_mat[, 2:5] / mom_mat[, 1]
orth_coef_haj <- mom_mat_haj[, 2:5] / mom_mat_haj[, 1]
orth_coef_lin <- mom_mat_lin[, 2:(5+4)] / mom_mat_lin[, 1]
orth_coef_haj_lin <- mom_mat_haj_lin[, 2:(5+4)] / mom_mat_haj_lin[, 1]










paste("orth_coef:", orth_coef)

#we compute the ground truth:
tau <- map_dbl(1:10000, ~{
  Z <- rbinom(n, size = 1, prob = r1); Y <- get_Y(Z)
  T_vec <- get_T(Z); D <- Y*T_vec/pscore1-Y*(1-T_vec)/pscore0
  return(mean(D))
}) %>% mean()

tau_haj <- map_dbl(1:10000, ~{
  Z <- rbinom(n, size = 1, prob = r1); Y <- get_Y(Z)
  T_vec <- get_T(Z); D <- Y*T_vec/(pscore1*mean(T_vec/pscore1))-Y*(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  # w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
  return(mean(D))
}) %>% mean()

sum <- c()

##################################################################################################################################bn = 3
sim_res<- map_dfr(1:10000, ~{
  
  sum <- c(sum,1); print("sum:"); print(length(sum))
  Z <- rbinom(n, size = 1, prob = r1); Y <- get_Y(Z); X_aug <- get_X(X,Z,G)
  T_vec <- get_T(Z)
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
  

  
  
  
  ###################################Ours_G_F#########################################################################
  X_db <- X_aug - w_haj * (orth_coef_haj)  #it is n*4;  n*1,  n*4
  D_2 <- (X_db * w)
  
  D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  
  V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%V) #here we should use the new variance estimator:
  
  Ours_G_haj <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  # var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
  var_Ours_G_haj <- t(V - D_2%*%hbeta_2_haj) %*%A %*% (V- D_2%*%hbeta_2_haj)/n^2 %>% as.vector()
  var_Ours_G_haj_naive <- t(V - D_2%*%hbeta_2_haj) %*% diag(n) %*% (V- D_2%*%hbeta_2_haj)/n^2 %>% as.vector()
  coverage_Ours_G_haj <- abs(Ours_G_haj - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj)
  coverage_Ours_G_haj_naive <- abs(Ours_G_haj - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_naive)

  
  
  ############################################Ours_X_F##########################################################################################
  
  X_db <- X  #it is n*4;  n*1,  n*4
  D_2 <- (X_db * w)
  # D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  

  
  hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
  Ours_X_haj <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  var_Ours_X_haj <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  var_Ours_X_haj_naive <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%diag(n) %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  coverage_Ours_X_haj <- abs(Ours_X_haj - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj)
  coverage_Ours_X_haj_naive <- abs(Ours_X_haj - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_naive)
  

  

  
  ###############################################Ours_G_lin###################################################################### 
  
  #Moreover, we use the lin's method!  G+Haj+lin； Lin的第二种
  X_db <- cbind(X_aug * T_vec, X_aug * (1-T_vec)) #这是初始的G
  X_db <- X_db - w_haj * (orth_coef_haj_lin)  #it is n*4;  n*1,  n*4
  # X_db <- X_db[, - which(colSums(abs(X_db)) == 0)]
  D_2 <- (X_db * w)
  
  
  D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  
  
  
  D <- Y * w
  
  V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%V) #here we should use the new variance estimator:
 
  Ours_G_haj_lin <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  var_Ours_G_haj_lin <- t(V- D_2%*%hbeta_2_haj) %*%A %*% (V- D_2%*%hbeta_2_haj )/n^2 %>% as.vector()
  var_Ours_G_haj_lin_naive <- t(V- D_2%*%hbeta_2_haj) %*% diag(n) %*% (V- D_2%*%hbeta_2_haj )/n^2 %>% as.vector()
  coverage_Ours_G_haj_lin <- abs(Ours_G_haj_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_lin)
  coverage_Ours_G_haj_lin_naive <- abs(Ours_G_haj_lin - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_lin_naive)
  
  
  ######################################################Ours_X_lin##################################################################
  #Moreover, we use the lin's method!  X+Haj+lin；lin的第四种
  X_db <- X #it is n*4;  n*1,  n*4
  X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
  #here we use...
  #X_db <- cbind(X * pscore1, X * pscore0)
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
  
  
  
  
  
  
  
  
  #we add the ablation study:
  ##################ablation study 1########################################################
  X_db <- cbind(X_aug * T_vec, X_aug * (1-T_vec)) #这是初始的G
  X_db <- X_db - w_haj * (orth_coef_haj_lin)  #it is n*4;  n*1,  n*4
  # X_db <- X_db[, -6]
  #########################Gao_L_ablation####################################################
  lm_haj <- lm(Y~1+T_vec+X_db, w = T_vec/pscore1+(1-T_vec)/pscore0)
  e_haj <- lm_haj %>% resid()
  Gao_L_abla <- lm_haj %>% coef() %>% .[2];
  # C_haj <- cbind(1,T_vec, T_vec*X)
  # var_Gao_L <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  
  # X_db <- cbind(X * (1-pscore0), X * (pscore0))
  hbeta_2_haj <- c(lm_haj %>% coef() %>% .[3],  lm_haj %>% coef() %>% .[4], lm_haj %>% coef() %>% .[5], lm_haj %>% coef() %>% .[6], lm_haj %>% coef() %>% .[7], lm_haj %>% coef() %>% .[8], lm_haj %>% coef() %>% .[9], lm_haj %>% coef() %>% .[10])
  # Ours_X_haj <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  w = T_vec/pscore1 - (1-T_vec)/pscore0
  D_2 <- X_db * w
  D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  var_Gao_L_abla <- t(D- D_2%*%hbeta_2_haj  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  var_Gao_L_naive_abla <- t(D- D_2%*%hbeta_2_haj  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%diag(n) %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # var_Gao_L_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%diag(n)%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  coverage_Gao_L_abla <- abs(Gao_L_abla-tau)<=qnorm(0.975)*sqrt(var_Gao_L_abla)
  coverage_Gao_L_naive_abla <- abs(Gao_L_abla-tau)<=qnorm(0.975)*sqrt(var_Gao_L_naive_abla)
  #########################Gao_L_ablation####################################################
  
  
  ####################################ablation study 2#######################
  
  ###########################Gao_F_ablation###################################
  X_db <- X_aug - w_haj * (orth_coef_haj)  #it is n*4;  n*1,  n*4
  D_2 <- (X_db * w)
  D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  
  lm_haj <- lm(Y~1+T_vec+X_db, w = T_vec/pscore1+(1-T_vec)/pscore0)
  e_haj <- lm_haj %>% resid()
  Gao_F_abla <- lm_haj %>% coef() %>% .[2]
  w <- T_vec/pscore1 - (1-T_vec)/pscore0
  # D_2 <- X_db * w
  hbeta_2_haj <- c(lm_haj %>% coef() %>% .[3], lm_haj %>% coef() %>% .[4], lm_haj %>% coef() %>% .[5],lm_haj %>% coef() %>% .[6]  )
  var_Gao_F_abla <- t(D- D_2 %*% hbeta_2_haj  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2 %*% hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  var_Gao_F_naive_abla <- t(D- D_2 %*% hbeta_2_haj  - (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%diag(n) %*% (D- D_2 %*% hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  # var_Gao_L_naive <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%diag(n)%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  coverage_Gao_F_abla <- abs(Gao_F_abla-tau)<=qnorm(0.975)*sqrt(var_Gao_F_abla)
  coverage_Gao_F_naive_abla <- abs(Gao_F_abla-tau)<=qnorm(0.975)*sqrt(var_Gao_F_naive_abla)
  ###########################Gao_F_ablation###################################
  
  
  
  ##########################ablation study 3######################################
  ##########################Ours_G_F_ablation#####################################
  X_db <- X_aug  #it is n*4;  n*1,  n*4
  D_2 <- (X_db * w)
  
  D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  
  V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%V) #here we should use the new variance estimator:
  Ours_G_haj_abla <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  # var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
  var_Ours_G_haj_abla <- t(V - D_2%*%hbeta_2_haj) %*%A %*% (V- D_2%*%hbeta_2_haj)/n^2 %>% as.vector()
  var_Ours_G_haj_naive_abla <- t(V - D_2%*%hbeta_2_haj) %*% diag(n) %*% (V- D_2%*%hbeta_2_haj)/n^2 %>% as.vector()
  coverage_Ours_G_haj_abla <- abs(Ours_G_haj_abla - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_abla)
  coverage_Ours_G_haj_naive_abla <- abs(Ours_G_haj_abla - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_naive_abla)
  ##########################Ours_G_F_ablation#####################################
  
  
  ##########################ablation study 4######################################
  ##########################Ours_G_L_ablation#####################################
  X_db <- cbind(X_aug * T_vec, X_aug * (1-T_vec)) #这是初始的G
  # X_db <- X_db[ , -6]
  #X_db <- X_db - w_haj * (orth_coef_haj_lin)  #it is n*4;  n*1,  n*4
  D_2 <- (X_db * w)
  D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
  D <- Y * w
  
  V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%V) #here we should use the new variance estimator:
  
  # quadratic_function <- function(hbeta) {
  #   V <- (D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)        )
  #   result <- t(V- D_2%*%hbeta ) %*% A %*% (V- D_2%*%hbeta)/n^2 %>% as.vector()
  #   result<- sqrt(result)
  #   return(result)
  # }
  # # 使用优化函数找到最小值和对应的位置
  # result <- optim( vector("logical", length = 2*4-1    ) * 1,          quadratic_function)
  # # # 输出最优值和最优位置
  # cat("最小值：", result$value, "\n")
  # hbeta_2_haj <- result$par
  # hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%(D-  (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) 
  # hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%(D-  (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)  )) #here we should use the new variance estimator:
  Ours_G_haj_lin_abla <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
  var_Ours_G_haj_lin_abla <- t(V- D_2%*%hbeta_2_haj) %*%A %*% (V- D_2%*%hbeta_2_haj )/n^2 %>% as.vector()
  var_Ours_G_haj_lin_naive_abla <- t(V- D_2%*%hbeta_2_haj) %*% diag(n) %*% (V- D_2%*%hbeta_2_haj )/n^2 %>% as.vector()
  coverage_Ours_G_haj_lin_abla <- abs(Ours_G_haj_lin_abla - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_lin_abla)
  coverage_Ours_G_haj_lin_naive_abla <- abs(Ours_G_haj_lin_abla - tau)<=qnorm(0.975)*sqrt(var_Ours_G_haj_lin_naive_abla)
  ##########################Ours_G_L_ablation#####################################
  
  
  
  return(tibble(Leung, Gao, Gao_F, Gao_L, Gao_F_abla, Gao_L_abla,     Ours_X_haj, Ours_G_haj, Ours_G_haj_abla,   Ours_X_haj_lin,Ours_G_haj_lin,Ours_G_haj_lin_abla,  
                var_Leung, var_Gao, var_Gao_F, var_Gao_L, var_Gao_F_abla, var_Gao_L_abla,      var_Ours_X_haj, var_Ours_G_haj, var_Ours_G_haj_abla,    var_Ours_X_haj_lin,  var_Ours_G_haj_lin, var_Ours_G_haj_lin_abla,
                coverage_Leung, coverage_Gao, coverage_Gao_F, coverage_Gao_L, coverage_Gao_F_abla, coverage_Gao_L_abla,       coverage_Ours_X_haj, coverage_Ours_G_haj, coverage_Ours_G_haj_abla,     coverage_Ours_X_haj_lin, coverage_Ours_G_haj_lin,coverage_Ours_G_haj_lin_abla,
                var_Leung_naive, var_Gao_naive, var_Gao_F_naive, var_Gao_L_naive, var_Gao_F_naive_abla, var_Gao_L_naive_abla,       var_Ours_X_haj_naive, var_Ours_G_haj_naive,var_Ours_G_haj_naive_abla,   var_Ours_X_haj_lin_naive,  var_Ours_G_haj_lin_naive, var_Ours_G_haj_lin_naive_abla,
                coverage_Leung_naive, coverage_Gao_naive, coverage_Gao_F_naive, coverage_Gao_L_naive,coverage_Gao_F_naive_abla, coverage_Gao_L_naive_abla,      coverage_Ours_X_haj_naive, coverage_Ours_G_haj_naive, coverage_Ours_G_haj_naive_abla,     coverage_Ours_X_haj_lin_naive, coverage_Ours_G_haj_lin_naive, coverage_Ours_G_haj_lin_naive_abla,
                
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

index <- 12

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
  variables <- c('Leung', 'Gao', 'Gao_F', 'Gao_L', 'Gao_F_abla', 'Gao_L_abla',  'Ours_G_haj',  'Ours_X_haj','Ours_G_haj_abla',  'Ours_G_haj_lin',  'Ours_X_haj_lin', 'Ours_G_haj_lin_abla')
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
  
  
  # o_coverage_Leung_plus <- mean(abs(sim_res$Leung - tau) <= qnorm(0.975) * sd(sim_res$Leung) )
  # o_coverage_Gao_naive_plus <- mean(abs(sim_res$Gao - tau) <= qnorm(0.975) * sd(sim_res$Gao) )
  # o_coverage_Gao_F_plus <- mean(abs(sim_res$Gao_F - tau) <= qnorm(0.975) * sd(sim_res$Gao_F) )
  # o_coverage_Gao_L_plus <- mean(abs(sim_res$Gao_L - tau) <= qnorm(0.975) * sd(sim_res$Gao_L) )
  # o_coverage_Ours_G_ht_plus <- mean(abs(sim_res$Ours_G_ht_plus - tau) <= qnorm(0.975) * sd(sim_res$Ours_G_ht_plus) )
  # o_coverage_Ours_G_haj_plus <- mean(abs(sim_res$Ours_G_haj_plus - tau) <= qnorm(0.975) * sd(sim_res$Ours_G_haj_plus) )
  # o_coverage_Ours_X_ht_plus <- mean(abs(sim_res$Ours_X_ht_plus - tau) <= qnorm(0.975) * sd(sim_res$Ours_X_ht_plus) )
  # o_coverage_Ours_X_haj_plus <- mean(abs(sim_res$Ours_X_haj_plus - tau) <= qnorm(0.975) * sd(sim_res$Ours_X_haj_plus) )
  # 
  # o_coverage_Ours_G_ht_plus_lin <- mean(abs(sim_res$Ours_G_ht_plus_lin - tau) <= qnorm(0.975) * sd(sim_res$Ours_G_ht_plus_lin) )
  # o_coverage_Ours_G_haj_plus_lin <- mean(abs(sim_res$Ours_G_haj_plus_lin - tau) <= qnorm(0.975) * sd(sim_res$Ours_G_haj_plus_lin) )
  # o_coverage_Ours_X_ht_plus_lin <- mean(abs(sim_res$Ours_X_ht_plus_lin - tau) <= qnorm(0.975) * sd(sim_res$Ours_X_ht_plus_lin) )
  # o_coverage_Ours_X_haj_plus_lin <- mean(abs(sim_res$Ours_X_haj_plus_lin - tau) <= qnorm(0.975) * sd(sim_res$Ours_X_haj_plus_lin) )
  
  # return(tibble(o_coverage_Leung_plus, o_coverage_Gao_naive_plus, o_coverage_Gao_F_plus, o_coverage_Gao_L_plus,  o_coverage_Ours_X_ht_plus, o_coverage_Ours_G_ht_plus,  o_coverage_Ours_X_ht_plus_lin, o_coverage_Ours_G_ht_plus_lin, o_coverage_Ours_X_haj_plus, o_coverage_Ours_G_haj_plus, o_coverage_Ours_X_haj_plus_lin, o_coverage_Ours_G_haj_plus_lin))
  return(tibble(oracle, oracle_plus))
  
})




# 指定要保存的文件路径
# file_path <- "/home/ZhihengZhang/1022new/result_synthetic.txt"
file_path <- "/home/ZhihengZhang/Final_response/syn/bn_3_result_synthetic_linear.txt"
# file_path <- "~/regression_interference_server/syn_total_new/result_synthetic.txt"

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
###########################################################################################################################################################################################################

#file_path <- "linear_parameter_X_final.txt"
#write.table(X, file = file_path, col.names = FALSE)
#file_path <- "linear_parameter_G_final.txt"
#write.table(G, file = file_path, col.names = FALSE)
#file_path <- "linear_parameter_noise_final.txt"
#write.table(errors, file = file_path, col.names = FALSE)
