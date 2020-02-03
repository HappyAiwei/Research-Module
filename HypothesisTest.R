
library(AER) ; library(foreign) ; library(mvtnorm); library(car); library(lmtest); 
library(zoo); library(sandwich); library(survival); library(ivmodel)
###################################################################################################################
# Just-identification with strong instruments
###################################################################################################################
rm(list=ls())
set.seed(100)

# number of observations
n <- 200
# error terms (epsilon and eta) correlate
sigma = matrix(c(0.25, 0.2, 0.2, 0.25),2,2)
mu <- c(0, 0) 
ee <- rmvnorm(n, mu, sigma) # [epsilon eta]
# intercept
Z0 <- rep(1, n)
X0 <- rep(1, n)
# iv
Z1 <- rnorm(n, mean=0, sd=1)

# first stage: X1 = pi0 * Z0 + pi1 * Z1 + eta
X1 <- 0*Z0 + 0.3*Z1 + ee[,2]
# second stage: Y = beta0 * X0 + beta1 * X1 + epsilon
Y <- 1*X0 + 1*X1 + ee[,1]

dat = data.frame(X1, Z0, Z1)

# unrestricted model
fs <- lm(X1 ~ Z0 + Z1 -1, data = dat)
# restricted model(iv excluded)
fn <- lm(X1 ~ Z0 -1, data = dat) 

# simple F-test
waldtest(fs, fn)$F[2]   # 79.257 > 10, valid instrument
waldtest(fs, fn)
###################################################################################################################
# Wald test

# Model 1: X1 ~ Z0 + Z1 - 1
# Model 2: X1 ~ Z0 - 1
# Res.Df Df      F    Pr(>F)    
# 1    198                        
# 2    199 -1 79.257 3.501e-16 ***
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
###################################################################################################################

f <- ivmodel(Y = Y, D = X1, Z = Z1, intercept = TRUE, beta0 = 1, alpha = 0.05) 
# F=0.9239091 < 3.8415, do not reject H0: beta = 1 
###################################################################################################################
# Anderson-Rubin test:
# F=0.9239091, df1=1, df2=198, p-value=0.33762
# 95 percent confidence interval:
# [ 0.634201774722229 , 1.08812328217801 ]
###################################################################################################################




###################################################################################################################
# Just-identification with weak instruments
###################################################################################################################
rm(list=ls())
set.seed(100)

n <- 200 # number of observations
# error terms (epsilon and eta) correlate
sigma = matrix(c(1, 0.9, 0.9, 1),2,2)
mu <- c(0, 0) 
ee <- rmvnorm(n, mu, sigma) # [epsilon eta]
# intercept
Z0 <- rep(1, n)
X0 <- rep(1, n)
# iv
Z1 <- rnorm(n, mean=0, sd=1)

# first stage: X1 = pi0 * Z0 + pi1 * Z1 + eta
X1 <- 0*Z0 + 0.2*Z1 + ee[,2]
# second stage: Y = beta0 * X0 + beta1 * X1 + epsilon
Y <- 1*X0 + 1*X1 + ee[,1]

dat = data.frame(X1, Z0, Z1)

# unrestricted model
fs <- lm(X1 ~ Z0 + Z1 -1, data = dat)
# restricted model(iv excluded)
fn <- lm(X1 ~ Z0 -1, data = dat) 

# simple F-test
waldtest(fs, fn)$F[2]   # 7.788 < 10, weak instrument
waldtest(fs, fn)
###################################################################################################################
# Wald test

# Model 1: X1 ~ Z0 + Z1 - 1
# Model 2: X1 ~ Z0 - 1
# Res.Df Df     F   Pr(>F)   
# 1    198                     
# 2    199 -1 7.788 0.005774 **
  # ---
  # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
###################################################################################################################

f <- ivmodel(Y = Y, D = X1, Z = Z1, intercept = TRUE, beta0 = 1, alpha = 0.05)
# F=0.7270333 < 3.8415, do not reject H0: beta = 1
###################################################################################################################
# Anderson-Rubin test:
# F=0.7270333, df1=1, df2=198, p-value=0.39488
# 95 percent confidence interval:
# [ -2.00969570441419 , 1.23288748965684 ]
###################################################################################################################




###################################################################################################################
# Over-identification with Strong instruments
###################################################################################################################
rm(list=ls())
set.seed(100)

n <- 200 # number of observations
K <- 16 # number of regressors of Z 
# error terms (epsilon and eta) correlate
sigma <- matrix(c(0.25, 0.1, 0.1, 0.25),2,2)
mu <- c(0, 0) 
ee <- rmvnorm(n, mu, sigma) # [epsilon eta]
# intercept
Z0 <- rep(1, n)
X0 <- rep(1, n)
# iv
Z1 <- matrix(data = rnorm(n*(K-1), mean=0, sd=1), nrow = n)

beta <- c(1, 1)
pi <- c(0, rep(0.3, (K-1)))

X1 <- cbind(Z0, Z1) %*% pi + ee[,2]
Y <- 1*X0 + 1*X1 + ee[,1]

f <- ivmodel(Y = Y, D = X1, Z = Z1, intercept = TRUE, beta0 = 1, alpha = 0.05, k = c(0, 1),heteroSE = FALSE)
# F=59.68835 > 11.51 when number of IV is 15. (Source: Stock and Yogo 2001.) 
# F=0.7932848 < 1.6664, do not reject H0: beta = 1
###################################################################################################################
# First Stage Regression Result:
  
# F=59.68835, df1=15, df2=184, p-value is < 2.22e-16
# R-squared=0.8295231,   Adjusted R-squared=0.8156255
# Residual standard error: 0.5107115 on 199 degrees of freedom
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

# Anderson-Rubin test:
# F=0.7932848, df1=15, df2=184, p-value=0.68413
# 95 percent confidence interval:
#[ 0.887372568377871 , 1.10983501294581 ]
###################################################################################################################





###################################################################################################################
# Over-identification with Weak instruments
###################################################################################################################
rm(list=ls())
set.seed(100)

n <- 200 # number of observations
K <- 16 # number of regressors of Z 
# error terms (epsilon and eta) correlate
sigma <- matrix(c(0.25, 0.2, 0.2, 0.25),2,2)
mu <- c(0, 0) 
ee <- rmvnorm(n, mu, sigma) # [epsilon eta]
# intercept
Z0 <- rep(1, n)
X0 <- rep(1, n)
# iv
Z1 <- matrix(data = rnorm(n*(K-1), mean=0, sd=1), nrow = n)

beta <- c(1, 1)
pi <- c(0, rep(0.1, (K-1)))

X1 <- cbind(Z0, Z1) %*% pi + ee[,2]
Y <- 1*X0 + 1*X1 + ee[,1]

f <- ivmodel(Y = Y, D = X1, Z = Z1, intercept = TRUE, beta0 = 1, alpha = 0.05, k = c(0, 1),heteroSE = FALSE)
# F=7.434647   < 11.51
# F=0.9357135 < 1.6664, do not reject H0: beta = 1
################################################################################################################### 
# First Stage Regression Result:
  
  # F=7.434647, df1=15, df2=184, p-value is 9.4413e-13
# R-squared=0.3773681,   Adjusted R-squared=0.32661
# Residual standard error: 0.491462 on 199 degrees of freedom
#_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

# Anderson-Rubin test:
# F=0.9357135, df1=15, df2=184, p-value=0.52572
# 95 percent confidence interval:
# [ 0.537978835651022 , 1.24973798711238 ]
################################################################################################################### 
