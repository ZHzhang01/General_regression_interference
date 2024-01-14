library(igraph)

n <- 400; r1 <- 0.5 
E <- sample_degseq(
  out.deg = ceiling(runif(n,min = 0, max = 4)), method = c("simple.no.multiple")
) %>%  as_adjacency_matrix()  %>% as.matrix() 
num_nb <- rowSums(E)
G <- E/rowSums(E)

# library(tidygraph)
# library(igraph)
# library(ggraph)
# graph <- graph_from_adjacency_matrix(E, mode = "directed")
# tbl_graph <- as_tbl_graph(graph)
# tbl_graph %>% ggraph() + geom_node_point() + geom_edge_link()  +  theme_graph()


X <-rnorm(n) %>% scale(scale = FALSE); epsilon <- rnorm(n)

# LLM model
get_Y <- function(Z){
  return(solve(-0.8*G+diag(n),-rep(1,times=n)+G%*%Z+Z+X+epsilon) %>% drop())
}
get_T <- function(Z){
  return(drop(E%*%Z<=floor(num_nb/2))*1)
}


# define exposure mapping as whether half of its neighbors is assigned to the treatment groups
pscore1 <- pbinom(floor(num_nb/2),size = num_nb, prob = r1); pscore0 <- 1-pscore1
A <- ((E%*%E)>0)*1; temp <- eigen(A);  A_p <- (temp$vectors)%*%diag((temp$values)*(temp$values>0))%*%solve(temp$vectors)


get_X <- function(X,Z,G){
  return(matrix(c(Z,drop(G%*%Z),X,drop(G%*%X)), nrow=n, ncol = 4)) 
}

get_orth_coef <- function(X,G, num_rep = 10000){
  mom_mat <- matrix(nrow = num_rep, ncol = 5)
  for(i in 1:num_rep){
    Z <- rbinom(n, size = 1, prob = r1);  X_aug <- get_X(X,Z,G)
    T_vec <- get_T(Z); w <- T_vec/pscore1-(1-T_vec)/pscore0
    mom_mat[i,] <- c(mean(w^2), colMeans(X_aug*w))
  }
  mom_mean <- mom_mat %>% colMeans()
  return(mom_mean[-1]/mom_mean[1])
}

orth_coef <- get_orth_coef(X,G) 

tau <- map_dbl(1:10000, ~{
  Z <- rbinom(n, size = 1, prob = r1); Y <- get_Y(Z)
  T_vec <- get_T(Z); D <- Y*T_vec/pscore1-Y*(1-T_vec)/pscore0; 
  tau_unadj_ht <- mean(D)
  return(tau_unadj_ht)
}) %>% mean()



sim_res<- map_dfr(1:1000, ~{
  Z <- rbinom(n, size = 1, prob = r1); Y <- get_Y(Z); X_aug <- get_X(X,Z,G)
  T_vec <- get_T(Z); D <- Y*T_vec/pscore1-Y*(1-T_vec)/pscore0; 
  w <- T_vec/pscore1-(1-T_vec)/pscore0
  tau_unadj_ht <- mean(D); 
  
  var_est_unadj_1 <- t(D-tau_unadj_ht)%*%A%*%(D-tau_unadj_ht)/n^2 %>% as.vector();
  var_est_unadj_2 <- t(D-tau_unadj_ht)%*%A_p%*%(D-tau_unadj_ht)/n^2 %>% as.vector()
  is_cover_unadj_1 <- abs(tau_unadj_ht-tau)<qnorm(0.975)*sqrt(var_est_unadj_1)
  is_cover_unadj_2 <- abs(tau_unadj_ht-tau)<qnorm(0.975)*sqrt(var_est_unadj_2)
  
  X_db <- X_aug-(w)%*%t(orth_coef)
  D_2 <- scale(X_db*w, scale = FALSE) 
  hbeta_1 <- solve(t(D_2)%*%A%*%(D_2),t(D_2)%*%A%*%(D-tau_unadj_ht))
  hbeta_2 <- solve(t(D_2)%*%A_p%*%(D_2),t(D_2)%*%A_p%*%(D-tau_unadj_ht))
  
  tau_adj_aug1 <- mean((Y-X_db%*%hbeta_1)*w)
  tau_adj_aug2 <- mean((Y-X_db%*%hbeta_2)*w)
  
  var_est_adj_aug1 <- t(D-D_2%*%hbeta_1-tau_adj_aug1)%*%A%*%(D-D_2%*%hbeta_1-tau_adj_aug1)/n^2 %>% as.vector()
  var_est_adj_aug2 <- t(D-D_2%*%hbeta_2-tau_adj_aug2)%*%A_p%*%(D-D_2%*%hbeta_2-tau_adj_aug2)/n^2 %>% as.vector()
  
  is_cover_adj_aug_1 <- abs(tau_adj_aug1-tau)<qnorm(0.975)*sqrt(var_est_adj_aug1)
  is_cover_adj_aug_2 <- abs(tau_adj_aug2-tau)<qnorm(0.975)*sqrt(var_est_adj_aug2)
  
  lm_haj <- lm(Y~1+T_vec+X+T_vec:X, w = T_vec/pscore1+(1-T_vec)/pscore0)
  e_haj <- lm_haj %>% resid(); tau_adj_haj <- lm_haj %>% coef() %>% .[2]
  C_haj <- cbind(1,T_vec,X,T_vec*X)
  var_est_adj_haj <- solve(t(C_haj)%*%diag(w)%*%C_haj)%*%(t(C_haj)%*%diag(w)%*%diag(e_haj)%*%A_p%*%diag(e_haj)%*%diag(w)%*%(C_haj))%*%solve(t(C_haj)%*%diag(w)%*%C_haj) %>% .[2,2]
  is_cover_adj_haj <- abs(tau_adj_haj-tau)<qnorm(0.975)*sqrt(var_est_adj_haj)
  
  return(tibble(tau_unadj_ht,tau_adj_aug1,tau_adj_aug2,tau_adj_haj, var_est_unadj_1, var_est_unadj_2, var_est_adj_aug1, var_est_adj_aug2, var_est_adj_haj, is_cover_unadj_1, is_cover_unadj_2 , is_cover_adj_aug_1, is_cover_adj_aug_2, is_cover_adj_haj))
})

sim_res %>% summarise_all(mean)  %>% as.data.frame()
sim_res %>% summarise_all(sd)  %>% as.data.frame()

# tau_unadj_haj <- mean(Y*T_vec/pscore1)/mean(T_vec/pscore1)-mean(Y*(1-T_vec)/pscore0)/mean((1-T_vec)/pscore0)