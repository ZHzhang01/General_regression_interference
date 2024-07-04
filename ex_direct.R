# First, we establish eligible Y; 1) attend the second-round session; 2) at least one friend attending the first round:
library(Matrix)
library(igraph)
library(tidyverse)
library(stringdist)






data <- data.frame(ex_name = X0422analysis$id, ex_delay = X0422analysis$delay, ex_intensive = X0422analysis$intensive, ex_buy = X0422analysis$takeup_survey)


###################add new covariate: village:########################
# # 根据 village 列创建新的 vilid 列
# data$vilid <- as.numeric(as.factor(data$village))
# # 对 vilid 列进行摘要统计
# summary(data$vilid)
# # 创建虚拟指标变量
# cluster <- model.matrix(~ vilid - 1, data = data)
# X0422analysis$vi_cluster <- cluster

# # 假设您有一个 n 维整数列向量 int_vector
# int_vector <- X0422analysis$vi_cluster  # 示例整数列向量
# # # 假设您有一个 n 维整数列向量 int_vector
# # int_vector <- c(25, 10, 33, 19)  # 示例整数列向量
# # 将整数列向量转换为 one-hot 编码
# one_hot_matrix <- matrix(0, nrow = length(int_vector), ncol = 47)
# for (i in 1:length(int_vector)) {
#   one_hot_matrix[i, int_vector[i]] <- 1
# }
# # 输出结果
# print(one_hot_matrix)
# X0422analysis$onehot_cluster <- one_hot_matrix
#here we add the onehot matrix already.


###########end: add new covariate: village####################


filtered_data <- data[data$ex_delay == 1& (data$ex_intensive == 0 | 1) & (data$ex_buy == 0|1), ] #select 1: attend the second round session;

print(filtered_data)
to_remove <- c()

X0422twoside_1 <- subset( X0422twoside, X0422twoside[,ncol(X0422twoside)] == 1) #optional select 2: we choose the double-link nodes;

user_data <- data.frame(UserID1 = X0422twoside$id, UserID2 = X0422twoside$network_id)

user_data <- user_data[complete.cases(user_data), ] #delete the NA term;


for (i in 1:nrow(filtered_data)) {
  target_id <- filtered_data$ex_name[i]
  neighbor_id <- user_data$UserID2[user_data$UserID1 == target_id]
  
  #对邻居节点进行预处理，筛选出在analysis文件的节点
  #case 1
  if (length(neighbor_id) > 0) {
    cat("UserID1:", target_id, "NeighborID:", neighbor_id, "\n")
    for (s in 1: length(neighbor_id)){
      if  (! (neighbor_id[s] %in% data$ex_name)){
        neighbor_id <- neighbor_id[ -neighbor_id[s] ]
      }
    }
    if(length(neighbor_id) == 0){
      cat("UserID1:", target_id, "has no neighbor in user_data2\n")
      to_remove <- c(to_remove, i)
    }
  }
  #case 2
  if(length(neighbor_id) == 0){
    cat("UserID1:", target_id, "has no neighbor in user_data2\n")
    to_remove <- c(to_remove, i)
  }
  
  
}

filtered_data <- filtered_data[-to_remove,]
#select 3: whether there exists neighbors attending the first round session:



to_remove <- c()

for (i in 1:nrow(filtered_data)){
  
  target_id <- filtered_data$ex_name[i]
  neighbor_id <- X0422twoside$network_id[X0422twoside$id == target_id]
  
  tempt <- 0
  # filtered_data_copy <- filtered_data
  # if (data$ex_delay[data$ex_name == target_id] == 0){
  #   tempt <- 1 #itself is in the fist round.
  # }
  
  for (s in 1: length(neighbor_id)){
    if (neighbor_id[s] %in% data$ex_name){
    if (data$ex_delay[data$ex_name == neighbor_id[s]] == 0){
      tempt <- 1
    }
       
    }
  }
  
  if (tempt == 0){
    to_remove <- c(to_remove, i)
  }

}

filtered_data <- filtered_data[-to_remove,]

print("we have done!")


# 判断两个ID是否同时存在于data2中的函数
check_ids_exists <- function(row) {
  all(row %in% filtered_data$ex_name)
}
# 对每一行进行判断，保留同时存在的行
adj_filter <- user_data[apply(user_data, 1, check_ids_exists), ]
# 输出最终结果
print(adj_filter)
#here is the adj matrix (sparse);
# sparse_adj <- sparseMatrix( i = adj_filter$UserID1, j = adj_filter$UserID2, x = c(rep(1,nrow(adj_filter)))   )
# 创建邻接矩阵
ex_adj_matrix <- matrix(0, nrow = nrow(filtered_data), ncol = nrow(filtered_data))
# ex_adj_matrix <- diag(rep(1,  nrow(filtered_data))).  # what should we choose?
# identity_mat <- diag(nrow(ex_adj_matrix))
# 填充邻接矩阵
for (i in 1:nrow(adj_filter)) {
  from_user <- adj_filter$UserID2[i]
  from_user_number <- which(filtered_data$ex_name == from_user)
  to_user <- adj_filter$UserID1[i]
  to_user_number <- which(filtered_data$ex_name == to_user)
  ex_adj_matrix[ to_user_number, from_user_number] <- 1
  # ex_adj_matrix[  from_user_number, to_user_number] <- 1
}
# 输出邻接矩阵
print(ex_adj_matrix) #目前仍然是单向网络；按照filtered_data的升序顺序；

# ex_adj_matrix <- ex_adj_matrix + identity_mat
# for (i in 1:nrow(ex_adj_matrix)){
#   ex_adj_matrix[i,i] <- 1
# }
#这里mengsi gao用的约束：friends是不是sample是个问题；这里用的是广义的friends.


matrix_data <- ex_adj_matrix # 示例矩阵数据
# 对矩阵进行对称化操作
symmetric_matrix <- matrix(0, nrow = nrow(matrix_data), ncol = ncol(matrix_data))  # 初始化对称化后的矩阵
for (i in 1:nrow(matrix_data)) {
  for (j in 1:ncol(matrix_data)) {
    symmetric_matrix[i, j] <- max(matrix_data[i, j], matrix_data[j, i])  # 将元素替换为 (i, j) 和 (j, i) 元素的最大值
  }
}
# 输出对称化后的矩阵
matrix_data <- symmetric_matrix
# 计算矩阵的行和
row_sums <- rowSums(matrix_data)
# 对非负整数矩阵进行行和归一化
normalized_matrix <- t((matrix_data, 1, function(x) {
  if (sum(x) > 0) {
    normalized_row <- x / sum(x)
  } else {
    normalized_row <- rep(0, length(x))
  }
  return(normalized_row)
}))
# 输出归一化后的矩阵
print(normalized_matrix)
G <- normalized_matrix




#now we extract the T = int:
pscore0 <- 0.5; pscore1 <- 0.5
#in the spillover effect, we should change it!



# sim_res<- map_dfr(1:1, ~{
# we compute the HT estimator:
n <- nrow(filtered_data)
T_vec <- filtered_data$ex_intensive; Y <- filtered_data$ex_buy
D <- Y*T_vec/pscore1-Y*(1-T_vec)/pscore0
Leung <- (mean(D))

D_haj <- Y*T_vec/(pscore1*mean(T_vec/pscore1))-Y*(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
# w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
Gau_naive <- (mean(D_haj))

#we construct the variance estimator:




#We start to record:


# #here it is right. HT:variance
# A <- ((ex_adj_matrix %*% ex_adj_matrix  %*% ex_adj_matrix )>0)*1; temp <- eigen(A);  A_p <- (temp$vectors)%*%diag((temp$values)*(temp$values>0))%*%solve(temp$vectors)
A <- ((symmetric_matrix %*% symmetric_matrix %*% symmetric_matrix)>0)*1; temp <- eigen(A);  A_p <- (temp$vectors)%*%diag((temp$values)*(temp$values>0))%*%solve(temp$vectors)
# A <- ((ex_adj_matrix)>0)*1; temp <- eigen(A);  A_p <- (temp$vectors)%*%diag((temp$values)*(temp$values>0))%*%solve(temp$vectors)
var_Leung <- t(D-Leung)%*%A_p%*%(D-Leung)/n^2 %>% as.vector()   #here A is the adjacent matrix. I have done here.
var_Leung <- sqrt(var_Leung)

#here it is right: Haj:variance
lm_haj <- lm(Y~1+T_vec, w = T_vec/pscore1+(1-T_vec)/pscore0)
e_haj <- lm_haj %>% resid()
C_haj <- cbind(1,T_vec); w <- T_vec/pscore1 + (1-T_vec)/pscore0
var_Gao_naive_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
var_Gao_naive_plus <- sqrt(var_Gao_naive_plus)
#add definition:
w_1 <- T_vec/pscore1
w_0 <- (1-T_vec)/pscore0
w_haj_1 <- T_vec/(pscore1*mean(T_vec/pscore1))
w_haj_0 <- (1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
var_Gao_naive_plus <- t(D -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D-(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Gao_naive_plus <- sqrt(var_Gao_naive_plus)
#the previous method:
# var_Gao_naive_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
# var_Gao_naive_plus <- sqrt(var_Gao_naive_plus)


#extract the covariates:
# agpop, male, educ_good,C rice_area

householdsize <- c()
ricearea <- c()
riceinc <- c()
edu <- c()
repay <- c()
understand <- c()
onehotvil <- c()

for (i in 1:nrow(filtered_data)) {
  target_id <- filtered_data$ex_name[i]
  
  agpop_element <- X0422analysis$agpop[X0422analysis$id == target_id];   householdsize <- c(householdsize, agpop_element); 
  rice_area_element <- X0422analysis$ricearea_2010[X0422analysis$id == target_id]; ricearea <- c(ricearea, rice_area_element); 
  rice_inc_element <- X0422analysis$rice_inc[X0422analysis$id == target_id] ;riceinc <- c(riceinc, rice_inc_element);
  edu_element <- X0422analysis$educ[X0422analysis$id == target_id] ; edu <- c(edu, edu_element);
  repay_element <- X0422analysis$insurance_repay[X0422analysis$id == target_id] ; repay <- c(repay, repay_element);
  understand_element <- X0422analysis$understanding[X0422analysis$id == target_id] ; understand <- c(understand, understand_element)
  onehotvil_element<- X0422analysis$onehot_cluster[X0422analysis$id == target_id, ]; onehotvil <- cbind(onehotvil, onehotvil_element)
}


# 计算中位数
householdsize[is.na(householdsize)] <- 0
median_householdsize <- median(householdsize, na.rm = TRUE)
householdsize[is.na(householdsize)] <- floor(median_householdsize)

ricearea[is.na(ricearea)] <- 0
median_ricearea <- median(ricearea, na.rm = TRUE)
ricearea[is.na(ricearea)] <- floor(median_ricearea)

riceinc[is.na(riceinc)] <- 0
median_riceinc <- median(riceinc, na.rm = TRUE)
riceinc[is.na(riceinc)] <- floor(median_riceinc)

edu[is.na(edu)] <- 0
median_edu <- median(edu, na.rm = TRUE)
edu[is.na(edu)] <- floor(median_edu)

repay[is.na(repay)] <- 0
median_repay <- median(repay, na.rm = TRUE)
repay[is.na(repay)] <- floor(median_repay)


understand[is.na(understand)] <- 0
median_understand <- median(understand, na.rm = TRUE)
understand[is.na(understand)] <- floor(median_understand)

# 对于小于中位数的取0，大于中位数的取1
householdsize_final <- ifelse(householdsize < median_householdsize, -1, 1)
ricearea_final <- ifelse(ricearea < median_ricearea, -1, 1)
riceinc_final <- ifelse(riceinc < median_riceinc, -1, 1)
edu_final <- ifelse(edu < median_edu, -1, 1)
repay_final <- ifelse(repay < median_edu, -1, 1)
understand_final <- ifelse(understand < median_edu, -1, 1)
# 
# householdsize_final <- scale(householdsize)
# ricearea_final <- scale(ricearea)
# repay_final <- scale(repay)
# repay_final <- scale(repay_final)
# understand_final <- scale(understand_final)


# X <- cbind(householdsize_final, ricearea_final, riceinc_final, edu_final, repay_final, understand_final)
X <- cbind(householdsize_final, ricearea_final, riceinc_final, edu_final, repay_final, understand_final)



# X <- cbind(understand_final)



to_remove <- c()
# X <- cbind(X, t(onehotvil))
X <- scale(X)

# for (s in 7:ncol(X)){
#   if (sum(X[,s] == 1) * 1 == 0){
#     to_remove <- c(to_remove, s)
#   }
# }
# X <- X[, -to_remove]
# X <- scale(X)
# X <- X[, 1:6]


#we have extracted the covariates; then we compute the Gao_{F}, Gao_{L}

#Gao_F:
lm_haj <- lm(Y~1+T_vec+X, w = T_vec/pscore1+(1-T_vec)/pscore0)
e_haj <- lm_haj %>% resid(); Gao_F <- lm_haj %>% coef() %>% .[2]
C_haj <- cbind(1,T_vec,X)
w = T_vec/pscore1+(1-T_vec)/pscore0
# var_Gao_F <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
var_Gao_F_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
var_Gao_F_plus <- sqrt(var_Gao_F_plus)
w = T_vec/pscore1-(1-T_vec)/pscore0
# D_2 <- scale(X*w, scale = FALSE)
D_2 <- X*w
beta_gao_f <- c()
for (i in (2+1):(2+ncol(X))){
  beta_gao_f <- cbind(beta_gao_f, lm_haj %>% coef() %>% .[i])
}
beta_gao_f <- matrix(beta_gao_f)
# beta_gao_f <- matrix(cbind( lm_haj %>% coef() %>% .[3],lm_haj %>% coef() %>% .[4],lm_haj %>% coef() %>% .[5],  lm_haj %>% coef() %>% .[6],lm_haj %>% coef() %>% .[7],lm_haj %>% coef() %>% .[8] ))
var_Gao_F_plus <- t(D- D_2%*%beta_gao_f  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%beta_gao_f  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Gao_F_plus <- sqrt(var_Gao_F_plus)

#here I give a tempt:














#Gao:L
w = T_vec/pscore1+(1-T_vec)/pscore0
lm_haj <- lm(Y~1+T_vec + X +T_vec*X, w = T_vec/pscore1+(1-T_vec)/pscore0)
e_haj <- lm_haj %>% resid(); Gao_L <- lm_haj %>% coef() %>% .[2]
C_haj <- cbind(1,T_vec, X,T_vec*X)
# var_Gao_L <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
var_Gao_L_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
var_Gao_L_plus <- sqrt(var_Gao_L_plus)
X_lin <- cbind(X * T_vec, X * (1-T_vec))
w = T_vec/pscore1-(1-T_vec)/pscore0
# D_2 <- scale(X_db*w, scale = FALSE)
D_2 <- X_lin*w
# T_vec_inverse <- (1 - T_vec)
# lm_haj_inverse <- lm(Y~1+T_vec_inverse+T_vec_inverse*X, w = T_vec_inverse/pscore1+(1-T_vec_inverse)/pscore0)
beta_gao_lin1 <- c()
for (i in (2+ncol(X)+1):(2+ncol(X)+ncol(X))){
  beta_gao_lin1 <- cbind(beta_gao_lin1, lm_haj %>% coef() %>% .[i])
}
beta_gao_lin2 <- c()
for (i in (2+1):(2+ncol(X))){
  beta_gao_lin2 <- cbind(beta_gao_lin2, lm_haj %>% coef() %>% .[i])
}
# beta_gao_lin1 <- matrix(cbind( lm_haj %>% coef() %>% .[9],lm_haj %>% coef() %>% .[10],lm_haj %>% coef() %>% .[11],  lm_haj %>% coef() %>% .[12],lm_haj %>% coef() %>% .[13],lm_haj %>% coef() %>% .[14] ))
# 
# beta_gao_lin2 <- matrix(cbind( lm_haj %>% coef() %>% .[3],lm_haj %>% coef() %>% .[4],lm_haj %>% coef() %>% .[5],  lm_haj %>% coef() %>% .[6],lm_haj %>% coef() %>% .[7],lm_haj %>% coef() %>% .[8] ))

beta_gao_lin3 <- beta_gao_lin2 + beta_gao_lin1

beta_gao_lin <- matrix(cbind(beta_gao_lin2, beta_gao_lin3))


# beta_gao_lin <- matrix(c( lm_haj %>% coef() %>% .[4], lm_haj_inverse %>% coef() %>% .[4]))

var_Gao_L_plus <- t(D- D_2%*%beta_gao_lin  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%beta_gao_lin  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Gao_L_plus <- sqrt(var_Gao_L_plus)









#we start to establish our method:

#we aim to calculate the coefficient:
get_X <- function(X,Z,G){
  # return(matrix(c(Z,drop(G%*%Z),X,drop(G%*%X)), nrow=n, ncol = 4))      #it is a generalized variable constructed by (Z, G*Z, X, G*X);    
  
  return(matrix(c(Z,drop(G%*%Z),     X,drop(G%*%X)), nrow=n, ncol =  2+ncol(X)*2 ))
}


#I have done here!!!!!
# mom_mat <- matrix(0, nrow = n, ncol = 1+1+ 2 +ncol(X)*2 )
# for(i in 1:1000){
#   Z <- rbinom(n, size = 1, prob = 0.5);  X_aug <- get_X(X,Z, G ) #in each simulation, we need compute the new $X_aug$ (n*4);
#   X_aug <- cbind(X_aug, 1)
#   w <- Z/pscore1-(1-Z)/pscore0 #They are both $n*1$ vectors;
#   mom_mat <- mom_mat + c(w^2, X_aug*w) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
# }
# orth_coef <- mom_mat[, 2: (1+1+ 2 +ncol(X)*2)] / mom_mat[, 1]
# #G can be self modified!

mom_mat <- matrix(0, nrow = n, ncol = 1+ 2 +ncol(X)*2  )
for(i in 1:1000){
  Z <- rbinom(n, size = 1, prob = 0.5)
  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
  w_haj <- Z/(pscore1*mean(Z/pscore1))-(1-Z)/(pscore0*mean((1-Z)/pscore0)) #They are both $n*1$ vectors;
  mom_mat <- mom_mat + c(w_haj^2, X_aug*w_haj) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
}
orth_coef_haj <- mom_mat[, 2: (1+2+ncol(X)*2)] / mom_mat[, 1]


############################################继续添加Lin的方法
#we also need to compute the iteraction debiasing procedure:
mom_mat <- matrix(0, nrow = n, ncol = 1+ (2 +ncol(X)*2)*2   )
for(i in 1:1000){
  Z <- rbinom(n, size = 1, prob = 0.5);  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
  w <- Z/pscore1-(1-Z)/pscore0 #They are both $n*1$ vectors;
  X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
  mom_mat <- mom_mat + c(w^2, X_aug_lin*w) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
}
orth_coef_lin <- mom_mat[, 2: (1+ (2 +ncol(X)*2)*2 ) ] / mom_mat[, 1]

mom_mat <- matrix(0, nrow = n, ncol = 1+ (2 +ncol(X)*2)*2  )
for(i in 1:1000){
  Z <- rbinom(n, size = 1, prob = r1);  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
  w_haj <- Z/(pscore1*mean(Z/pscore1))-(1-Z)/(pscore0*mean((1-Z)/pscore0)) #They are both $n*1$ vectors;
  X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
  mom_mat <- mom_mat + c(w_haj^2, X_aug_lin*w_haj) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
}
orth_coef_haj_lin <- mom_mat[, 2: (1+ (2 +ncol(X)*2)*2 )] / mom_mat[, 1]








#T_vec不能变!

w <- T_vec/pscore1-(1-T_vec)/pscore0
w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
#add definition:
w_1 <- T_vec/pscore1
w_0 <- (1-T_vec)/pscore0
w_haj_1 <- T_vec/(pscore1*mean(T_vec/pscore1))
w_haj_0 <- (1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))

#We start to construct our general auxiliary methods:
# X_aug <- get_X(X,T_vec,G)
# X_aug <- cbind(X_aug, 1)
# X_db <- X_aug - (w) * (orth_coef)  #it is n*4;  n*1,  n*4
# # D_2 <- scale(X_db*w, scale = FALSE)
# D_2 <- X_db * w
# # hbeta_2 <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-Leung)) #here we should use the new variance estimator:
# hbeta_2 <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-  (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) 
# Ours_G_ht_plus <- mean((Y-X_db%*%hbeta_2)*w) 

X_aug <- get_X(X,T_vec,G)
X_db <- X_aug - w_haj * (orth_coef_haj)  #it is n*4;  n*1,  n*4
D_2 <- X_db * w
hbeta_2_haj <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
Ours_G_haj_plus <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
# var_Ours_G_ht_plus <- t(D-D_2%*%hbeta_2-Leung)%*%A_p%*%(D-D_2%*%hbeta_2-Leung)/n^2 %>% as.vector()
# var_Ours_G_ht_plus <- sqrt(var_Ours_G_ht_plus)
var_Ours_G_haj_plus <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Ours_G_haj_plus <- sqrt(var_Ours_G_haj_plus)

X_aug <- get_X(X,T_vec,G)
X_db <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
X_db <- X_db - w_haj * orth_coef_haj_lin
#维度：1+1+col(X)+col(X); 1+1+col(X)+col(X); 
#之后如果有全零的列要删除，找到所有元素均为0的列索引
zero_cols_idx <- apply(X_db, 2, function(x) all(x == 0))
# 删除所有元素均为0的列并输出剩下的矩阵
X_db <- X_db[, !zero_cols_idx]
#here we construct the new X_db;
# X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
# D_2 <- scale(X_db*w, scale = FALSE)
D_2 <- (X_db*w)
# hbeta_2_haj <- c(hbeta_2_haj , hbeta_2_haj )
hbeta_2_haj <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:

Ours_G_haj_plus_lin <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
var_Ours_G_haj_plus_lin <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Ours_G_haj_plus_lin <- sqrt(var_Ours_G_haj_plus_lin)

# # 定义二次函数
# quadratic_function <- function(hbeta_2_haj) {
#   var_Ours_G_haj_plus_lin <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
#   var_Ours_G_haj_plus_lin <- sqrt(var_Ours_G_haj_plus_lin)
#   return(var_Ours_G_haj_plus_lin)
# }
# # 宕值函数
# objective_function <- function(x) {
#   return(quadratic_function(x))
# }
# # 设置优化问题：找到在 x 全部在 [-0.1, 0.1] 范围内的最小值点
# result <- optim(rep(0, 8+20), fn = objective_function, lower = rep(-0.2, 8+20), upper = rep(0.2, 8+20))
# # 输出最小值和对应的输入值
# cat("最小值:", result$value, "\n")
# cat("最小值点:", result$par, "\n")
# Ours_G_haj_plus_lin <- mean((Y-X_db%*%result$par)*w_haj)
# var_Ours_G_haj_plus_lin <- result$value











# hbeta_2_haj_1 <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)  * T_vec)) #here we should use the new variance estimator:
# hbeta_2_haj_0 <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)  * (1-T_vec)) )
# 
# Ours_G_haj_plus_lin <- mean((Y-X_db%*%hbeta_2_haj_1)*w_haj * T_vec +  (Y-X_db%*%hbeta_2_haj_0)*w_haj * (1-T_vec) )
# tempt_mat <- (D- D_2%*%hbeta_2_haj_1  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )*T_vec + (D- D_2%*%hbeta_2_haj_0  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )*(1-T_vec)
# var_Ours_G_haj_plus_lin <- t(tempt_mat) %*%A %*% (tempt_mat)/n^2 %>% as.vector()
# var_Ours_G_haj_plus_lin <- sqrt(var_Ours_G_haj_plus_lin)













#We also consider G == X:
# X_db <- X - (w) * (orth_coef[,3])  #it is n*4;  n*1,  n*4
X_db <- X 
# D_2 <- scale(X_db*w, scale = FALSE) 
X_db <- cbind(X_db, 1)
D_2 <- X_db * w
hbeta_2 <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D-  (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0)   )) #here we should use the new variance estimator:
Ours_X_ht_plus <- mean((Y-X_db%*%hbeta_2)*w) 

# X_db <- X - w_haj * (orth_coef_haj[,3])  #it is n*4;  n*1,  n*4
X_db <- X 
# D_2 <- scale(X_db*w, scale = FALSE) 
D_2 <- X_db * w
hbeta_2_haj <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
Ours_X_haj_plus <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
#I have done here: compute the var and coverage:
# var_Ours_G_ht <- t(D-D_2%*%hbeta_1-Ours_G_ht)%*%A%*%(D-D_2%*%hbeta_1-Ours_G_ht)/n^2 %>% as.vector()
# var_Ours_X_ht_plus <- t(D-D_2%*%hbeta_2-Ours_X_ht_plus)%*%A_p%*%(D-D_2%*%hbeta_2-Ours_X_ht_plus)/n^2 %>% as.vector()
# var_Ours_X_ht_plus <- sqrt(var_Ours_X_ht_plus)
# var_Ours_X_ht_plus <- t(D-D_2%*%hbeta_2-Leung)%*%A_p%*%(D-D_2%*%hbeta_2-Leung)/n^2 %>% as.vector()
var_Ours_X_haj_plus <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Ours_X_haj_plus <- sqrt(var_Ours_X_haj_plus)


# coverage_Ours_G_ht <- abs(Ours_G_ht-tau)<=qnorm(0.975)*sqrt(var_Ours_G_ht)
# coverage_Ours_X_ht_plus <- abs(Ours_X_ht_plus - tau)<=qnorm(0.975)*sqrt(var_Ours_X_ht_plus)
# coverage_Ours_X_haj_plus <- abs(Ours_X_haj_plus - tau)<=qnorm(0.975)*sqrt(var_Ours_X_haj_plus)
X_db <- X 
X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
# D_2 <- scale(X_db*w, scale = FALSE) 
D_2 <- X_db * w
hbeta_2_haj <- solve(t(D_2)%*%A_p%*%(D_2),   t(D_2)%*%A_p%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ))
Ours_X_haj_plus_lin <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
var_Ours_X_haj_plus_lin <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A_p %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Ours_X_haj_plus_lin <- sqrt(var_Ours_X_haj_plus_lin)


# res <- (tibble(Leung, Gau_naive, Gao_F, Gao_L, Ours_X_ht_plus, Ours_X_haj_plus, Ours_G_ht_plus , Ours_G_haj_plus,Ours_X_haj_plus_lin,Ours_G_haj_plus_lin, var_Leung, var_Gao_naive_plus, var_Gao_F_plus,  var_Gao_L_plus,  var_Ours_X_ht_plus, var_Ours_X_haj_plus, var_Ours_G_ht_plus,  var_Ours_G_haj_plus, var_Ours_X_haj_plus_lin, var_Ours_G_haj_plus_lin))


res <- (tibble(Leung, Gau_naive, Gao_F, Gao_L,Ours_X_haj_plus,  Ours_G_haj_plus,Ours_X_haj_plus_lin,Ours_G_haj_plus_lin, var_Leung, var_Gao_naive_plus, var_Gao_F_plus,  var_Gao_L_plus, var_Ours_X_haj_plus, var_Ours_G_haj_plus, var_Ours_X_haj_plus_lin, var_Ours_G_haj_plus_lin))


print(res, n = Inf, width = Inf)





