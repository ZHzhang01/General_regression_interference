
# Paths below are set relative to the root directory of the repository.
# Please adjust them as needed if your local working directory is different.
repo_root <- "."

load(file.path(repo_root, "ex", "703.RData"))


#We replicate the ex_spillover with village variable:
# First, we establish eligible Y; 1) attend the second-round session; 2) at least one friend attending the first round:
library(Matrix)
library(igraph)
library(tidyverse)
library(stringdist)
control <- list(abstol = 1e-10, reltol = 1e-10) 



# X0422analysis <- X0422analysis[complete.cases(X0422analysis),  ] 
data <- data.frame(ex_name = X0422analysis$id, ex_delay = X0422analysis$delay, ex_intensive = X0422analysis$intensive, ex_buy = X0422analysis$takeup_survey, ex_new =  X0422analysis$insurance_buy)
# data <- data[complete.cases(data), ]


filtered_data <- data[data$ex_delay == 1& (data$ex_intensive == 0 | 1) & (data$ex_buy == 0|1),  ]

# 
print(filtered_data)

to_remove <- c()

X0422twoside_1 <- subset( X0422twoside, X0422twoside[,ncol(X0422twoside)] == 1) #remain the undirected two-sided graph

user_data <- data.frame(UserID1 = X0422twoside$id, UserID2 = X0422twoside$network_id)

user_data <- user_data[complete.cases(user_data), ]

user_data_new <- data.frame(UserID1 = X0422twoside$id, UserID2 = X0422twoside$network_id)

user_data_new <- user_data_new[complete.cases(user_data_new), ]
#We still use the X0422twoside;

for (i in 1:nrow(filtered_data)) {
  target_id <- filtered_data$ex_name[i]
  neighbor_id <- user_data$UserID2[user_data$UserID1 == target_id]
  

  #case 1
  if (length(neighbor_id) > 0) {
    cat("UserID1:", target_id, "NeighborID:", neighbor_id, "\n")
    for (s in 1: length(neighbor_id)){
      if  (! (neighbor_id[s] %in% data$ex_name)){
        neighbor_id <- neighbor_id[ -neighbor_id[s] ]
      }
    }
    if(length(neighbor_id) == 0 ){
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
#should check whether there exists neighbors attending the first round session:



to_remove <- c()


for (i in 1:nrow(filtered_data)){
  
  
  target_id <- filtered_data$ex_name[i]
  neighbor_id <- X0422twoside$network_id[X0422twoside$id == target_id]

  tempt <- 0
  tempt_2 <- 0
  # filtered_data_copy <- filtered_data
  for (s in 1: length(neighbor_id)){
    if (neighbor_id[s] %in% data$ex_name){
      if (data$ex_delay[data$ex_name == neighbor_id[s]] == 0){
        tempt <- 1
        tempt_2 <- tempt_2 + 1
      }
      # tempt <- 1
    }
    
    # if (data$ex_delay[data$ex_name == target_id] == 0){
    #   tempt <- 1 #itself is in the fist round.
    # }
    
  }
  
  if (tempt == 0){
    to_remove <- c(to_remove, i)
  }
  
  if (tempt_2 == length(neighbor_id))
    to_remove <- c(to_remove, i)
  
}

filtered_data <- filtered_data[-to_remove,]


# to_remove <- c()
# 
# for (i in 1:nrow(filtered_data)){
#   
#   target_id <- filtered_data$ex_name[i]
#   neighbor_id <- X0422twoside$network_id[X0422twoside$id == target_id]
#   if (length(neighbor_id) >= 5){
#     to_remove <- c(to_remove, i)
#   } 
#   
# }
# filtered_data <- filtered_data[-to_remove,]


print("we have done!")




check_ids_exists <- function(row) {
  all(row %in% filtered_data$ex_name)
}

adj_filter <- user_data[apply(user_data, 1, check_ids_exists), ]

print(adj_filter)

#here is the adj matrix (sparse);
# sparse_adj <- sparseMatrix( i = adj_filter$UserID1, j = adj_filter$UserID2, x = c(rep(1,nrow(adj_filter)))   )

ex_adj_matrix <- matrix(0, nrow = nrow(filtered_data), ncol = nrow(filtered_data))  #注意这里的维度;这是真正使用的邻接矩阵；关注的是这些真正strong的网络
# identity_mat <- diag(nrow(ex_adj_matrix))

for (i in 1:nrow(adj_filter)) {
  from_user <- adj_filter$UserID2[i]
  from_user_number <- which(filtered_data$ex_name == from_user)
  to_user <- adj_filter$UserID1[i]
  to_user_number <- which(filtered_data$ex_name == to_user)
  ex_adj_matrix[ to_user_number, from_user_number] <- 1
  # ex_adj_matrix[ from_user_number, to_user_number] <- 1
}

print(ex_adj_matrix) 

# ex_adj_matrix <- ex_adj_matrix + identity_mat
# for (i in 1:nrow(ex_adj_matrix)){
#   ex_adj_matrix[i,i] <- 1
# }



# ex_var_matrix <- matrix(0, nrow = nrow(filtered_data), ncol = nrow(filtered_data))
# for (i in nrow(filtered_data)){
#   for (j in nrow(filtered_data)){
#     ex_var_matrix[i,j] <- max(ex_adj_matrix[i,j],   ex_adj_matrix[j,i])
#   }
# }
# ex_var_matrix <- ex_adj_matrix + t(ex_adj_matrix) -diag(nrow(ex_adj_matrix))
# G_ours <- ex_var_matrix/rowSums(ex_var_matrix)
#here is the normalized matrix for augmentation:


matrix_data <- ex_adj_matrix 



symmetric_matrix <- matrix(0, nrow = nrow(matrix_data), ncol = ncol(matrix_data))  
for (i in 1:nrow(matrix_data)) {
  for (j in 1:ncol(matrix_data)) {
    symmetric_matrix[i, j] <- max(matrix_data[i, j], matrix_data[j, i])  
  }
}

matrix_data <- symmetric_matrix

row_sums <- rowSums(matrix_data)

normalized_matrix <- t(apply(matrix_data, 1, function(x) {
  if (sum(x) > 0) {
    normalized_row <- x / sum(x)
  } else {
    normalized_row <- rep(0, length(x))
  }
  return(normalized_row)
}))

print(normalized_matrix)
G <- normalized_matrix













# sim_res<- map_dfr(1:1, ~{
# we compute the HT estimator:
n <- nrow(filtered_data)
# series_1 <- filtered_data$ex_intensive; series_2 <- 1-filtered_data$ex_delay
# series <- series_1 * series_2

T_vec <- numeric( n )
pscore0 <- numeric(n)
sum <- 0
for (i in 1: nrow(filtered_data)){
  target_id <- filtered_data$ex_name[i]
  neighbor_id <- user_data$UserID2[user_data$UserID1 == target_id]
  # neighbor_id_inter <- intersect(neighbor_id, X0422analysis$id)
  # neighbor_id_inter <- intersect(neighbor_id_inter, X0422allinforawnet$id)
  elig <- 0
  for (s in 1: length(neighbor_id)){
    if  (! (neighbor_id[s] %in% data$ex_name)){
      neighbor_id <- neighbor_id[ -neighbor_id[s] ]
    } else {
      if (data$ex_delay[data$ex_name == neighbor_id[s] ] == 0)
      {
        elig <- (elig + 1)
      }
    }
   
  }
  
  # elig <- min(elig, 4)
  # pscore0[i] <- pbinom(0 ,size = length(neighbor_id) - rowSums(ex_adj_matrix), prob = 0.5)
  pscore0[i] <- pbinom(0 ,size = elig, prob = 0.5)
  # pscore0[i] <- max(pscore0[i], 0.1)
  # pscore0[i]<-0.5

  # pscore0[i] <- pbinom(0 ,size = (rowSums(ex_adj_matrix)[i]-1), prob = 0.25)

  if (length(neighbor_id) > 0){
  for (s in 1: length(neighbor_id)){
    # tempt <- 0
    if (neighbor_id[s] %in% data$ex_name){
      if (data$ex_delay[data$ex_name == neighbor_id[s]] == 0 & data$ex_intensive[data$ex_name == neighbor_id[s]] == 1){
        T_vec[i] <- 1
        sum <- sum+1
      }
      # if (tempt >= floor( length(neighbor_id)/2 ) ){T_vec[i] <- 1}
      # tempt <- 1
    }
  }}
  
}



pscore1 <- 1-pscore0
print(sum)
#I have done here;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Y <- filtered_data$ex_buy

D <- Y*T_vec/pscore1-Y*(1-T_vec)/pscore0
Leung <- (mean(D))

D_haj <- Y*T_vec/(pscore1*mean(T_vec/pscore1))-Y*(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
# w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
Gau_naive <- (mean(D_haj))

#we construct the variance estimator:




#We start to record:


# #here it is right. HT:variance
# A <- ((ex_adj_matrix %*% ex_adj_matrix %*% ex_adj_matrix )>0)*1; temp <- eigen(A);  A <- (temp$vectors)%*%diag((temp$values)*(temp$values>0))%*%solve(temp$vectors)


g <- graph_from_adjacency_matrix(symmetric_matrix, mode = "undirected")

shortest_paths_mat <- shortest.paths(g, mode = "all")

print(shortest_paths_mat)

bn <- 3
A <- (shortest_paths_mat <= bn)*1; temp <- eigen(A);  A_p <- (temp$vectors)%*%diag((temp$values)*(temp$values>0))%*%solve(temp$vectors)
# A <- ((symmetric_matrix %*% symmetric_matrix %*% symmetric_matrix)>0)*1; temp <- eigen(A);  A_p <- (temp$vectors)%*%diag((temp$values)*(temp$values>0))%*%solve(temp$vectors)


var_Leung <- t(D-Leung)%*%A%*%(D-Leung)/n^2 %>% as.vector()   #here A is the adjacent matrix. I have done here.
var_Leung <- sqrt(var_Leung)

#here it is right: Haj:variance
lm_haj <- lm(Y~1+T_vec, w = T_vec/pscore1+(1-T_vec)/pscore0)
e_haj <- lm_haj %>% resid()
C_haj <- cbind(1,T_vec); w <- T_vec/pscore1-(1-T_vec)/pscore0
# var_Gao_naive_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
# var_Gao_naive_plus <- sqrt(var_Gao_naive_plus)
#add definition:
w_1 <- T_vec/pscore1
w_0 <- (1-T_vec)/pscore0
w_haj_1 <- T_vec/(pscore1*mean(T_vec/pscore1))
w_haj_0 <- (1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
var_Gao_naive_plus <- t(D -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D-(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Gao_naive_plus <- sqrt(var_Gao_naive_plus)



#extract the covariates:
# agpop, male, educ_good,C rice_area

householdsize <- c()
ricearea <- c()
riceinc <- c()
edu <- c()
repay <- c()
understand <- c()

for (i in 1:nrow(filtered_data)) {
  target_id <- filtered_data$ex_name[i]
  
  agpop_element <- X0422analysis$agpop[X0422analysis$id == target_id];   householdsize <- c(householdsize, agpop_element);
  rice_area_element <- X0422analysis$ricearea_2010[X0422analysis$id == target_id]; ricearea <- c(ricearea, rice_area_element);
  rice_inc_element <- X0422analysis$rice_inc[X0422analysis$id == target_id] ;riceinc <- c(riceinc, rice_inc_element);
  edu_element <- X0422analysis$educ[X0422analysis$id == target_id] ; edu <- c(edu, edu_element);
  repay_element <- X0422analysis$insurance_repay[X0422analysis$id == target_id] ; repay <- c(repay, repay_element);
  understand_element <- X0422analysis$understanding[X0422analysis$id == target_id] ; understand <- c(understand, understand_element)
}



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


X <- cbind(householdsize_final, ricearea_final, riceinc_final, edu_final, repay_final, understand_final)
# X <- cbind( ricearea_final )
# X <- cbind(understand_final)

# filtered_data <- data[data$ex_delay == 1& (data$ex_intensive == 0 | 1) & (data$ex_buy == 0|1), ]

cluster_record <- X0422analysis$onehot_cluster[X0422analysis$id %in% filtered_data$ex_name == 1, ]
zero_cols_idx <- apply(cluster_record, 2, function(x) all(x == 0))

cluster_record <- cluster_record[, !zero_cols_idx]
col_sums <- colSums(cluster_record)

top_five_cols <- tail(order(col_sums), 40)
cluster_record <- cluster_record[, top_five_cols]

X <- cbind(X, cluster_record)
X <- cbind(X)

# X <- X[, 1:6]
X <- scale(X)

village_num <- c()

for (i in 1: nrow(filtered_data)){
  count <- X0422analysis$vi_cluster[X0422analysis$id == filtered_data$ex_name[i]]
  village_num <- c(village_num, count)
}

ourG <- cbind(X, village_num)
ourG <- scale(ourG)

village_num <- c()

for (i in 1: nrow(filtered_data)){
  count <- X0422analysis$vi_cluster[X0422analysis$id == filtered_data$ex_name[i]]  #记录村庄序号
  village_num <- c(village_num, count)
}



vil_people_rate <- c()
for (i in 1:47){
  if (sum(village_num == i) > 0 ){
    vil_people_D <- filtered_data$ex_intensive[village_num == i] 
    vil_people_D <- sum(vil_people_D)/sum(village_num == i)
  }
  else{
    vil_people_D <- 0
    
  }
  vil_people_rate <- c(vil_people_rate, vil_people_D)
}

rate <- c()
for (i in 1:nrow(filtered_data)){
  count <- X0422analysis$vi_cluster[X0422analysis$id == filtered_data$ex_name[i]]  #记录村庄序号
  rate <- c(rate, vil_people_rate[count])
}

rate2 <- c()
rate3 <- c()
rate4 <- c()
for (i in 1:nrow(filtered_data)){
  rate2 <- c(rate2,   X0422analysis$network_rate_preintensive[X0422analysis$id == filtered_data$ex_name[i]] )
  rate3 <- c(rate3,   X0422analysis$network_rate_presession[X0422analysis$id == filtered_data$ex_name[i]] )
  rate4 <- c(rate4,   X0422analysis$network_rate_presimple[X0422analysis$id == filtered_data$ex_name[i]] )
}

# new_G <- cbind(X, rate2, rate3)
new_G <- cbind(X, rate2, rate3, filtered_data$ex_intensive)
# new_G <- scale(new_G)


new_G <- cbind(X, rate2, rate3)








#Gao_F
lm_haj <- lm(Y~1+T_vec+X, w = T_vec/pscore1+(1-T_vec)/pscore0)
e_haj <- lm_haj %>% resid(); Gao_F <- lm_haj %>% coef() %>% .[2]
C_haj <- cbind(1,T_vec,X)
w = T_vec/pscore1+(1-T_vec)/pscore0
#I have done here::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# var_Gao_F <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
# var_Gao_F_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
# var_Gao_F_plus <- sqrt(var_Gao_F_plus)
w = T_vec/pscore1-(1-T_vec)/pscore0
# D_2 <- scale(X*w, scale = FALSE)
D_2 <- X*w
beta_gao_f <- c()
for (i in (2+1):(2+ncol(X))){
  beta_gao_f <- cbind(beta_gao_f, lm_haj %>% coef() %>% .[i])
}
beta_gao_f[is.na(beta_gao_f)] <- 0
beta_gao_f <- matrix(beta_gao_f)
# beta_gao_f <- matrix(cbind( lm_haj %>% coef() %>% .[3],lm_haj %>% coef() %>% .[4],lm_haj %>% coef() %>% .[5],  lm_haj %>% coef() %>% .[6],lm_haj %>% coef() %>% .[7],lm_haj %>% coef() %>% .[8] ))
var_Gao_F_plus <- t(D- D_2%*%beta_gao_f  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2%*%beta_gao_f  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Gao_F_plus <- sqrt(var_Gao_F_plus)




#Gao:L
w = T_vec/pscore1+(1-T_vec)/pscore0
lm_haj <- lm(Y~1+T_vec + X +T_vec*X, w = T_vec/pscore1+(1-T_vec)/pscore0)
e_haj <- lm_haj %>% resid(); Gao_L <- lm_haj %>% coef() %>% .[2]
C_haj <- cbind(1,T_vec, X,T_vec*X)
# var_Gao_L <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
# var_Gao_L_plus <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
# var_Gao_L_plus <- sqrt(var_Gao_L_plus)
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
beta_gao_lin2[is.na(beta_gao_lin2)] <- 0
beta_gao_lin3[is.na(beta_gao_lin3)] <- 0
beta_gao_lin <- matrix(cbind(beta_gao_lin3, beta_gao_lin2))
# beta_gao_lin <- matrix(c( lm_haj %>% coef() %>% .[4], lm_haj_inverse %>% coef() %>% .[4]))
var_Gao_L_plus <- t(D- D_2%*%beta_gao_lin  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2%*%beta_gao_lin  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Gao_L_plus <- sqrt(var_Gao_L_plus)













#we start to establish our method:

#we aim to calculate the coefficient:
get_X <- function(X,Z,G){
  # return(matrix(c(Z,drop(G%*%Z),X,drop(G%*%X)), nrow=n, ncol = 4))      #it is a generalized variable constructed by (Z, G*Z, X, G*X);
  
  return(matrix(c(Z,drop(G%*%Z),     X, drop(G%*%X)), nrow=n, ncol = 1+1+ncol(X) * 2))
}

get_T <- function(Z){
  TZ <- numeric( nrow(filtered_data) )
  X0422analysis$tempt_Z <- Z
  for (i in 1: nrow(filtered_data)){
    target_id <- filtered_data$ex_name[i]
    neighbor_id <- user_data$UserID2[user_data$UserID1 == target_id]
    
    

    for (s in 1: length(neighbor_id)){
      if (neighbor_id[s] %in% X0422analysis$id){
        if ( X0422analysis$tempt_Z[X0422analysis$id == neighbor_id[s] ] ==1 & X0422analysis$delay[X0422analysis$id == neighbor_id[s] ] == 0  )
        {TZ[i] <- 1}
      }
    }
    
  }
  
  
  
  return(TZ)
}

get_rate <- function(Z){
  rate <- numeric(nrow(filtered_data))
  X0422analysis$tempt_Z <- Z
  for (i in 1: nrow(filtered_data)){
    target_id <- filtered_data$ex_name[i]
    neighbor_id <- user_data$UserID2[user_data$UserID1 == target_id]
    
    

    rate[i] <- sum( X0422analysis$tempt_Z[X0422analysis$id %in% neighbor_id] * (1-X0422analysis$delay[X0422analysis$id %in% neighbor_id] )) / length(neighbor_id)
    # rate_2[i] <- sum((1-X0422analysis$delay[X0422analysis$id %in% neighbor_id] )) / length(neighbor_id)
    
    
  }
  return(rate)
  
}

get_rate_2 <- function(Z){
  rate_2 <- numeric(nrow(filtered_data))
  X0422analysis$tempt_Z <- Z
  for (i in 1: nrow(filtered_data)){
    target_id <- filtered_data$ex_name[i]
    neighbor_id <- user_data$UserID2[user_data$UserID1 == target_id]
    
    
    
    # rate[i] <- sum( X0422analysis$tempt_Z[X0422analysis$id %in% neighbor_id] * (1-X0422analysis$delay[X0422analysis$id %in% neighbor_id] )) / length(neighbor_id)
    rate_2[i] <- sum((1-X0422analysis$delay[X0422analysis$id %in% neighbor_id] )) / length(neighbor_id)
    
    
  }
  return(rate_2)
  
}




Z <- rbinom(nrow(X0422analysis), size = 1, prob = 0.5)
tempt_Z <- Z
X0422analysis <- cbind(X0422analysis, tempt_Z)




# mom_mat <- matrix(0, nrow = n, ncol = 1+1 + 2+2*ncol(X))
# for(i in 1:10000){
#   Z <- rbinom(nrow(X0422analysis), size = 1, prob = 0.25)
#   X0422analysis$tempt_Z <- Z
#   TZ <- get_T(Z)
#
#   subset_tempt <- X0422analysis[X0422analysis$id %in% filtered_data$ex_name,]
#   Z_sub <- subset_tempt$tempt_Z
#
#   X_aug <- get_X(X,Z_sub, G ) #in each simulation, we need compute the new $X_aug$ (n*4);
#   X_aug <- cbind(X_aug, 1)
#   w <- TZ/pscore1-(1-TZ)/pscore0 #They are both $n*1$ vectors;
#   mom_mat <- mom_mat + c(w^2, X_aug*w) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
# }
# orth_coef <- mom_mat[, 2: (1+1 + 2+2*ncol(X))] / mom_mat[, 1]
#G can be self modified!

mom_mat <- matrix(0, nrow = n, ncol =  1 + ncol(new_G) )
mom_mat_lin <- matrix(0, nrow = n, ncol =  1 + (ncol(new_G))*2 )
# mom_mat_new <- matrix(0, nrow = n, ncol =  1 + 2+2*ncol(ourG) )
for(i in 1:10000){
  cat("iteration:", i)
  Z <- rbinom(nrow(X0422analysis), size = 1, prob = 0.5)
  X0422analysis$tempt_Z <- Z
  TZ <- get_T(Z)
  
  subset_tempt <- X0422analysis[X0422analysis$id %in% filtered_data$ex_name,]
  Z_sub <- subset_tempt$tempt_Z
  w_haj <- TZ/(pscore1*mean(TZ/pscore1))-(1-TZ)/(pscore0*mean((1-TZ)/pscore0)) #They are both $n*1$ vectors;
  
  # X_aug <- new_G #in each simulation, we need compute the new $X_aug$ (n*4);

  X_aug <- cbind(X, get_rate(Z), get_rate_2(Z), Z_sub)
  X_aug <- cbind(X, get_rate(Z), get_rate_2(Z))
  X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
  mom_mat <- mom_mat + c(w_haj^2, X_aug*w_haj) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
  mom_mat_lin <- mom_mat_lin + c(w_haj^2, X_aug_lin*w_haj)
  # X_aug_new <- get_X(ourG, Z_sub, G)
  # mom_mat_new <- mom_mat_new + c(w_haj^2, X_aug_new*w_haj)
}
orth_coef_haj <- mom_mat[, 2: (1 + ncol(new_G))] / mom_mat[, 1]
orth_coef_haj_lin <- mom_mat_lin[, 2:(1+ (ncol(new_G) )*2 )] / mom_mat_lin[, 1]

# mom_mat <- matrix(0, nrow = n, ncol =  1 + (ncol(new_G))*2 )
# # mom_mat_new <- matrix(0, nrow = n, ncol =  1 + (2+2*ncol(ourG))*2 )
# for(i in 1:1000){
#   cat("iteration:", i)
#   Z <- rbinom(nrow(X0422analysis), size = 1, prob = 0.5)
#   X0422analysis$tempt_Z <- Z
#   TZ <- get_T(Z)
#   
#   subset_tempt <- X0422analysis[X0422analysis$id %in% filtered_data$ex_name,]
#   Z_sub <- subset_tempt$tempt_Z
#   w_haj <- TZ/(pscore1*mean(TZ/pscore1))-(1-TZ)/(pscore0*mean((1-TZ)/pscore0)) #They are both $n*1$ vectors;
#   
#   # X_aug <- new_G #in each simulation, we need compute the new $X_aug$ (n*4);
#   X_aug <- cbind(X, get_rate(Z), get_rate_2(Z), Z_sub)
#   X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
#   mom_mat <- mom_mat + c(w_haj^2, X_aug_lin*w_haj) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
#   
#   # X_aug_new <- get_X(ourG,Z_sub, G ) #in each simulation, we need compute the new $X_aug$ (n*4);
#   # X_aug_lin_new <- cbind(X_aug_new * T_vec, X_aug_new * (1-T_vec))
#   # mom_mat_new <- mom_mat_new + c(w_haj^2, X_aug_lin_new*w_haj) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
#   # 
#   
#   
# }
# orth_coef_haj_lin <- mom_mat[, 2:(1+ (ncol(new_G) )*2 )] / mom_mat[, 1]
# # orth_coef_haj_lin_new <- mom_mat_new[, 2:(1+ (2+2*ncol(ourG) )*2 )] / mom_mat[, 1]

# mom_mat <- matrix(0, nrow = n, ncol = 1+ (2+2*ncol(X) )*2           )
# for(i in 1:10000){
#   Z <- rbinom(n, size = 1, prob = r1);  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
#   T_vec <- get_T(Z); w <- T_vec/pscore1-(1-T_vec)/pscore0 #They are both $n*1$ vectors;
#   X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
#   mom_mat <- mom_mat + c(w^2, X_aug_lin*w) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
# }
# orth_coef_lin <- mom_mat[, 2:(1+(2+2*ncol(X) )*2 )] / mom_mat[, 1]
#
# mom_mat <- matrix(0, nrow = n, ncol = 1+ (2+2*ncol(X) )*2      )
# for(i in 1:10000){
#   Z <- rbinom(n, size = 1, prob = r1);  X_aug <- get_X(X,Z,G) #in each simulation, we need compute the new $X_aug$ (n*4);
#   T_vec <- get_T(Z); w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0)) #They are both $n*1$ vectors;
#   X_aug_lin <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
#   mom_mat <- mom_mat + c(w_haj^2, X_aug_lin*w_haj) # for each simulation process, the left is $w^2$ (n*1 vector), the right is $(w * X_aug)$ (n*4 vector);
# }
# orth_coef_haj_lin <- mom_mat[, 2:(1+ (2+2*ncol(X) )*2 )] / mom_mat[, 1]












w <- T_vec/pscore1-(1-T_vec)/pscore0
w_haj <- T_vec/(pscore1*mean(T_vec/pscore1))-(1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))
#add definition:
w_1 <- T_vec/pscore1
w_0 <- (1-T_vec)/pscore0
w_haj_1 <- T_vec/(pscore1*mean(T_vec/pscore1))
w_haj_0 <- (1-T_vec)/(pscore0*mean((1-T_vec)/pscore0))


#we start to construct our own methods!






#third case
# X_db <- X - w_haj * (orth_coef_haj[,3])  #it is n*4;  n*1,  n*4
X_db <- X
# D_2 <- scale(X_db*w, scale = FALSE)
D_2 <- X_db*w
#D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
zero_cols_idx <- apply(X_db, 2, function(x) all(x == 0))

X_db <- X_db[, !zero_cols_idx]
D_2 <- X_db * w
D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
quadratic_function <- function(hbeta) {
  result <- t(D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*% A %*% (D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  result<- sqrt(result)
  return(result)
}

result <- optim( beta_gao_f,          quadratic_function, control = control)

cat("最小值：", result$value, "\n")
cat("最优位置：", result$par, "\n")
hbeta_2_haj_5 <- result$par
# hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
Ours_X_haj_plus <- mean((Y-X_db%*%hbeta_2_haj_5)*w_haj)
# var_Ours_X_haj_plus <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
# var_Ours_X_haj_plus <- sqrt(var_Ours_X_haj_plus)
var_Ours_X_haj_plus <- result$value
print(var_Ours_X_haj_plus)


#fourth case:
X_aug <- X #we revise it!
X_db <- cbind(X_aug * T_vec, X_aug * (1-T_vec))
#X_db <- cbind(X_aug * pscore1, X_aug * (1-pscore1))
# X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
# D_2 <- scale(X_db*w, scale = FALSE)
# zero_cols_idx <- apply(X_db, 2, function(x) all(x == 0))

# X_db <- X_db[, !zero_cols_idx]
D_2 <- X_db * w
D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
quadratic_function <- function(hbeta) {
  result <- t(D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*% A %*% (D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  result<- sqrt(result)
  return(result)
}

result <- optim( beta_gao_lin,          quadratic_function, control = control)

cat("最小值：", result$value, "\n")
cat("最优位置：", result$par, "\n")
hbeta_2_haj_4 <- result$par
var_Ours_X_haj_plus_lin <- result$value
Ours_X_haj_plus_lin <- mean((Y-X_db%*%hbeta_2_haj_4)*w_haj)
print(Ours_X_haj_plus_lin)



###########################################################################construct the new G!
#third case
# X_db <- X - w_haj * (orth_coef_haj[,3])  #it is n*4;  n*1,  n*4
X_db <- new_G
X_db <- X_db - w_haj * (orth_coef_haj) 
# X_db[, 1:ncol(X)] <- X[, 1:ncol(X)]
# D_2 <- scale(X_db*w, scale = FALSE)
D_2 <- X_db*w
D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))

# zero_cols_idx <- apply(X_db, 2, function(x) all(x == 0))

# X_db <- X_db[, !zero_cols_idx]
# D_2 <- X_db * w
quadratic_function <- function(hbeta) {
  result <- t(D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*% A %*% (D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  result<- sqrt(result)
  return(result)
}

#result <- optim( matrix(c(hbeta_2_haj_5,rep(0,3)) ),          quadratic_function)
result <- optim( matrix(c(hbeta_2_haj_5,rep(0,2)) ),          quadratic_function)

cat("最小值：", result$value, "\n")
cat("最优位置：", result$par, "\n")
hbeta_2_haj <- result$par
# hbeta_2_haj <- solve(t(D_2)%*%A%*%(D_2),   t(D_2)%*%A%*%(D- (mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )) #here we should use the new variance estimator:
Ours_newG_haj_plus <- mean((Y-X_db%*%hbeta_2_haj)*w_haj)
var_Ours_newG_haj_plus <- t(D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*%A %*% (D- D_2%*%hbeta_2_haj  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
var_Ours_newG_haj_plus <- sqrt(var_Ours_newG_haj_plus)
print(var_Ours_newG_haj_plus)





X_db <- new_G #we revise it!
X_db <- cbind(X_db * T_vec, X_db * (1-T_vec))
X_db <- X_db - w_haj * (orth_coef_haj_lin) 

# X_db <- X_db[, !zero_cols_idx]
D_2 <- X_db * w
D_2 <- D_2 - (as.matrix(w_1) %*%  t(as.matrix(colMeans(X_db*w_haj_1)) ) - as.matrix(w_0) %*%  t(as.matrix(colMeans(X_db*w_haj_0)) ))
quadratic_function <- function(hbeta) {
  result <- t(D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) ) %*% A %*% (D- D_2%*%hbeta  -(mean(Y*w_haj_1)*w_1-mean(Y*w_haj_0)*w_0) )/n^2 %>% as.vector()
  result<- sqrt(result)
  return(result)
}


#result0 <- optim( matrix(c( hbeta_2_haj_4[1:(ncol(X)), ], rep(0,3),  hbeta_2_haj_4[(ncol(X)+1):(ncol(X)*2), ], rep(0, 3) ))    ,    quadratic_function, control = control)
result0 <- optim( matrix(c( hbeta_2_haj_4[1:(ncol(X)), ], rep(0,2),  hbeta_2_haj_4[(ncol(X)+1):(ncol(X)*2), ], rep(0, 2) ))    ,    quadratic_function, control = control)

cat("min:", result0$value, "\n")
cat("min:", result0$par, "\n")
hbeta_2_haj_4 <- result0$par
hbeta_2_haj_4 <- matrix(hbeta_2_haj_4)
var_Ours_newG_haj_plus_lin <- result0$value
Ours_newG_haj_plus_lin <- mean((Y-X_db%*%hbeta_2_haj_4)*w_haj)
print(Ours_newG_haj_plus_lin)







































res <- tibble(Leung, Gau_naive, Gao_F, Gao_L, Ours_X_haj_plus, Ours_newG_haj_plus,  Ours_X_haj_plus_lin,Ours_newG_haj_plus_lin,  var_Leung, var_Gao_naive_plus, var_Gao_F_plus,  var_Gao_L_plus,  
              var_Ours_X_haj_plus, var_Ours_newG_haj_plus, var_Ours_X_haj_plus_lin, var_Ours_newG_haj_plus_lin)



print(res, n = Inf, width = Inf)







file_path <- file.path(repo_root, "ex", "bn_3_ex_spillover_A.txt")

write.table(res, file = file_path, col.names = FALSE)
                       
cat("数据已成功写入文本文件:", file_path, "\n")



