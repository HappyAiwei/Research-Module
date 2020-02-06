##-------------------------------------------------------------------------------
## This code is written by Aiwei Huang.                                         -
## Licensed under The Apache License Version 2.0 [see LICENSE for details]      -
##-------------------------------------------------------------------------------


library(AER); library(foreign); library(mvtnorm); library(car); library(lmtest); 
library(zoo); library(sandwich); library(survival)
library(MASS); library(matlib); library(expm); library(depth)
library(ggplot2); library(plot3D); library(plotly); library(depth); library(ggpubr)


## Parameters
# K:number of regressors of Z 
# n:number of observations
r <- 1000  # number of repetition
L <- 2 # number of regressors of X
mu <- c(0, 0)
beta <- c(1, 1)
obs <- c(25, 50, 100, 200, 400, 800)
list_beta <- rep(list(matrix(data = NA, nrow = L, ncol = 1)), r)

###########################################################################################################
## Data Generate Process
###########################################################################################################
dgp <- function(K, L, n, r, mu, sigma, pi, beta){
  list_residuals <- rep(list(matrix(data = NA, nrow = n, ncol = 2)), r)
  list_Z <- rep(list(matrix(data = NA, nrow = n, ncol = K)), r)
  list_X <- rep(list(matrix(data = NA, nrow = n, ncol = L)), r)
  list_Y <- rep(list(matrix(data = NA, nrow = n, ncol = 1)), r)
  for (i in 1:r){
    list_residuals[[i]] <- as.matrix(mvrnorm(n, mu, sigma))
    list_Z[[i]] <- cbind(matrix(rep(1, n), nrow = n), 
                         matrix(data = rnorm(n*(K-1), mean=0, sd=1), nrow = n, ncol = (K-1)))
    list_X[[i]] <- cbind(matrix(rep(1, n), nrow = n), 
                         list_Z[[i]] %*% pi + matrix(list_residuals[[i]][, 2],nrow = n))
    list_Y[[i]] <- list_X[[i]] %*% beta + matrix(list_residuals[[i]][, 1],nrow = n)
  }
  return(list(list_Z, list_X, list_Y, list_residuals))
}


###########################################################################################################
## OLS estimators 
###########################################################################################################
ols <- function(list_X, list_Y){
  for (i in 1:r){
    list_beta[[i]] <- inv(t(list_X[[i]]) %*% list_X[[i]]) %*% t(list_X[[i]]) %*% list_Y[[i]]
  }
  return(list_beta)
}

###########################################################################################################
## 2SLS estimators
###########################################################################################################
tsls <- function(list_X, list_Y, list_Z, n){
  list_Pz <- rep(list(matrix(data = NA, nrow = n, ncol = n)), r)
  for (i in 1:r){
    list_Pz[[i]] <- list_Z[[i]] %*%  inv(t(list_Z[[i]]) %*% list_Z[[i]]) %*% t(list_Z[[i]])
    list_beta[[i]] <- inv(t(list_X[[i]]) %*% list_Pz[[i]] %*% list_X[[i]]) %*% 
      t(list_X[[i]]) %*% list_Pz[[i]] %*% list_Y[[i]]
  }
  return(list_beta)
}

###########################################################################################################
## LIML estimators
###########################################################################################################
liml <- function(list_X, list_Y, list_Z, list_residuals, n, beta){
  list_k <- rep(NA, r)
  list_rhs <- rep(list(matrix(data = NA, nrow = n, ncol = 1)), r)         # RHS of structural form
  list_Mz <- rep(list(matrix(data = NA, nrow = n, ncol = n)),  r)         # Projects off the predetermined variables
  list_Mw <- rep(list(matrix(data = NA, nrow = n, ncol = n)), r)          # Projects off all the instruments
  for (i in 1:r){
    list_rhs[[i]] <- as.matrix(list_Z[[i]][, 1]) %*% beta[1] + list_residuals[[i]][ ,1] 
    list_Mz[[i]] <- diag(n) - as.matrix(list_Z[[i]][, 1]) %*% 
      (t(as.matrix(list_Z[[i]][, 1])) %*% as.matrix(list_Z[[i]][, 1]))^(-1) %*% 
      t(as.matrix(list_Z[[i]][, 1]))
    list_Mw[[i]] <-  diag(n) - list_Z[[i]]  %*% inv(t(list_Z[[i]]) %*% list_Z[[i]]) %*% t(list_Z[[i]])
    list_k[i] <- (t(list_rhs[[i]]) %*% list_Mz[[i]] %*% list_rhs[[i]])/
      (t(list_rhs[[i]]) %*% list_Mw[[i]] %*% list_rhs[[i]])
    list_beta[[i]] <- inv(t(list_X[[i]]) %*% (diag(n) - list_k[i] * list_Mw[[i]]) %*% list_X[[i]]) %*%
      t(list_X[[i]]) %*% (diag(n) - list_k[i] * list_Mw[[i]]) %*% list_Y[[i]]
  }
  return(list_beta)
}

###########################################################################################################
## Jack-Knife estimators
###########################################################################################################
jackknife <- function(list_X, list_Y, list_Z, K, n){
  list_X_jive1 <- rep(list(matrix(data = NA, nrow = n, ncol = L-1)), r)
  list_X_jive <- rep(list(matrix(data = NA, nrow = n, ncol = L)), r)
  list_pi_hat <- rep(list(matrix(data = NA, nrow = K, ncol = L-1)), r)
  h <- matrix(data = NA, nrow = n, ncol = r)
  for (i in 1:r){
    list_pi_hat[[i]] <- inv(t(list_Z[[i]]) %*% list_Z[[i]]) %*% t(list_Z[[i]]) %*% list_X[[i]][,2]
    
    for (j in 1:n){
      h[j, i] <- t(list_Z[[i]][j,]) %*% inv(t(list_Z[[i]]) %*% list_Z[[i]]) %*% list_Z[[i]][j,]
      list_X_jive1[[i]][j,] <- (t(list_Z[[i]][j,]) %*% list_pi_hat[[i]] - h[j, i] * list_X[[i]][j, 2])/(1 - h[j, i])
    }
    
    list_X_jive[[i]] <- cbind(matrix(rep(1, n), nrow = n),list_X_jive1[[i]])
    list_beta[[i]] <-  inv(t(list_X_jive[[i]]) %*% list_X[[i]]) %*% t(list_X_jive[[i]]) %*% list_Y[[i]]
  }
  return(list(list_beta, list_X_jive)) 
}

###########################################################################################################
## Coverage Probability
###########################################################################################################
coverage_probability <- function(list_X, list_Y, list_beta, n, r){
  sigma_hat_sq <- matrix(data = NA, nrow = 1, ncol = r)           # sample variance of error terms
  se_beta <- rep(list(matrix(data = NA, nrow = 2, ncol = 2)), r)  # standard error of the estimators
  ci_beta <- rep(list(matrix(data = NA, nrow = 2, ncol = 2)), r)  # confidence intervals of the estimators
  count <- 0  # counting times that b1=1 locate in the 95% confidence interval of estimated b1 in repetitions

  for(i in 1:r){
    sigma_hat_sq[1,i] <- 1/n * t(list_Y[[i]]) %*% 
      (diag(n) - list_X[[i]] %*% inv(t(list_X[[i]]) %*% list_X[[i]]) %*% t(list_X[[i]])) %*% list_Y[[i]]
    
    se_beta[[i]] <-sqrtm(sigma_hat_sq[1,i] * inv(t(list_X[[i]]) %*% list_X[[i]]))

    ci_beta[[i]][2, 1]<- list_beta[[i]][2, 1] - 1.96 * se_beta[[i]][2, 2]  #left_b1
    ci_beta[[i]][2, 2]<- list_beta[[i]][2, 1] + 1.96 * se_beta[[i]][2, 2]  #right_b1
    
    if(ci_beta[[i]][2, 1] <= 1 && 1 <= ci_beta[[i]][2, 2]){count <- count + 1}
    }
  
  return(count/r)
}


## Simulation Data Generation:
###########################################################################################################
# Figure 1
###########################################################################################################
data_f1 <- function(n){
  
  f1_rho <- seq(from = 0, to = 1, by = 0.01)
  K <- 7 #number of overidentifying restriction(5) + L(2)
  f1_pi <- rbind(matrix(data = c(0), ncol = 1), matrix(rep(sqrt(0.1/6), K-1), ncol = 1)) # pi0 = 0, pi1,..., pi6 are equal
  f1_cov <- matrix(data = NA, nrow = 1, ncol = length(f1_rho))
  f1_sigma <- rep(list(matrix(data = NA, nrow = 2, ncol = 2)), length(f1_rho)) # variance-covariance matrixs
  data_list <- list()
  
  for (i in 1:length(f1_rho)){
    f1_cov[i] <- f1_rho[i]*sqrt(0.9)
    f1_sigma[[i]]<- matrix(data = c(1, f1_cov[i], f1_cov[i], 0.9), nrow = 2, ncol = 2)
    data <- dgp(K, L, n, r, mu, sigma = f1_sigma[[i]], pi = f1_pi, beta)
    data_list <- append(data_list, data)
  }
  return(data_list)
}

###########################################################################################################
# Figure 2
###########################################################################################################
data_f2 <- function(n){
  
  a <- seq(from = 0.01, to = 1, by = 0.01)
  K <- 7
  f2_rho <- 0.9
  data_list <- list()
  f2_pi <- rep(list(matrix(data = NA, nrow = K, ncol = 1)), length(a))
  f2_cov <- matrix(data = NA, nrow = 1, ncol = length(a))
  f2_sigma <- rep(list(matrix(data = NA, nrow = 2, ncol = 2)), length(a))
  
  for (i in 1:length(a)){
    f2_pi[[i]] <- rbind(matrix(data = c(0), ncol = 1), matrix(rep(sqrt(a[i]/6), K-1), ncol = 1)) 
    f2_cov[i] <- f2_rho * sqrt(1 - a[i])
    f2_sigma[[i]][1, 1] <- 1  
    f2_sigma[[i]][1, 2] <- f2_cov[i]
    f2_sigma[[i]][2, 1] <- f2_cov[i]
    f2_sigma[[i]][2, 2] <- 1 - a[i]
    
    data <- dgp(K, L, n, r, mu, sigma = f2_sigma[[i]], pi = f2_pi[[i]], beta)
    data_list <- append(data_list, data)
  }
  return(data_list)
}

###########################################################################################################
# Figure 3
###########################################################################################################
data_f3 <- function(n){
  
  o_r <- seq(from = 0, to = 16, by = 1) # numbers of overidentifying restrictions
  K3 <- o_r + L
  cov_ee <- 0.9 * sqrt(0.9) # covariance of epsilon and eta
  f3_sigma <- matrix(data = c(1, cov_ee, cov_ee, 0.9), nrow = 2, ncol = 2)
  pi_list <- list()
  data_list <- list()
  
  for (i in 1:length(K3)){
    pi <- rbind(matrix(data = c(0), ncol = 1), matrix(rep(sqrt(0.1/(K3[i] - 1)), K3[i] - 1), ncol = 1))
    pi_list[[i]]<- pi
    data <- dgp(K = K3[i], L, n, r, mu, sigma = f3_sigma, pi = pi_list[[i]], beta)
    data_list <- append(data_list, data)
  }
  return(data_list)
}



###########################################################################################################
## Median Bias Calculation: 
###########################################################################################################

# For figure1, figure2:
MedianBias <- function(data, n, variate, K){
  # variate1 <- f1_rho <- seq(from = 0, to = 1, by = 0.01)
  # variate2 <- a <- seq(from = 0.01, to = 1, by = 0.01)
  # variate3 <- K3 <- seq(from = 0, to = 16, by = 1) + L
  
  mb_ols <- mb_2sls <- mb_liml <- mb_jive <- matrix(data = NA, nrow = length(variate), ncol = 1)
  d <- data
  
  # Median Bias
  for(i in 1:length(variate)){
    mb_ols[i] <- median(sapply(ols(d[[2]], d[[3]]), '[[', 2)) - 1 
    mb_2sls[i] <- median(sapply(tsls(d[[2]], d[[3]], d[[1]], n), '[[', 2)) - 1  
    mb_liml[i] <- median(sapply(liml(d[[2]], d[[3]], d[[1]], d[[4]], n, beta = c(1, 1) ), '[[', 2)) - 1
    mb_jive[i] <- median(sapply(jackknife(d[[2]], d[[3]], d[[1]], K, n)[[1]], '[[', 2)) - 1
    d <- d[- c(1, 2, 3, 4)]
  }
  return(list(mb_ols, mb_2sls, mb_liml, mb_jive)) 
}  


# For figure3:
MedianBias_f3 <- function(data, n, variate, K){
  # variate1 <- f1_rho <- seq(from = 0, to = 1, by = 0.01)
  # variate2 <- a <- seq(from = 0.01, to = 1, by = 0.01)
  # variate3 <- K3 <- seq(from = 0, to = 16, by = 1) + L
  
  mb_ols <- mb_2sls <- mb_liml <- mb_jive <- matrix(data = NA, nrow = length(variate), ncol = 1)
  d <- data
  
  # Median Bias
  for(i in 1:length(variate)){
    mb_ols[i] <- median(sapply(ols(d[[2]], d[[3]]), '[[', 2)) - 1 
    mb_2sls[i] <- median(sapply(tsls(d[[2]], d[[3]], d[[1]], n), '[[', 2)) - 1  
    mb_liml[i] <- median(sapply(liml(d[[2]], d[[3]], d[[1]], d[[4]], n, beta = c(1, 1) ), '[[', 2)) - 1
    mb_jive[i] <- median(sapply(jackknife(d[[2]], d[[3]], d[[1]], K[i], n)[[1]], '[[', 2)) - 1
    d <- d[- c(1, 2, 3, 4)]
  }
  return(list(mb_ols, mb_2sls, mb_liml, mb_jive)) 
}  