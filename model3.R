##-------------------------------------------------------------------------------
## This code is written by Aiwei Huang.                                         -
## Licensed under The Apache License Version 2.0 [see LICENSE for details]      -
##-------------------------------------------------------------------------------

rm(list=ls())
set.seed(123)
source("RM_functions.R")

#########################################################################################################
### Model 3 : Over-identification Strong IV
#########################################################################################################

## Parameters
K <- 16 # number of regressors of Z 
n <- 200 # number of observations
r <- 5000  # number of repetition
L <- 2 # number of regressors of X
# error terms (epsilon and eta) correlate
sigma <- matrix(c(0.25, 0.1, 0.1, 0.25),2,2)
mu <- c(0, 0)
beta <- c(1, 1)
pi <- c(0, rep(0.3, (K-1)))
list_beta <- rep(list(matrix(data = NA, nrow = L, ncol = 1)), r)

data_os <- dgp(K, L, n, r, mu, sigma, pi, beta)
#ols <- function(list_X, list_Y)
ols_os <- ols(data_os[[2]], data_os[[3]])
#tsls <- function(list_X, list_Y, list_Z, n)
tsls_os <- tsls(data_os[[2]], data_os[[3]], data_os[[1]], n)
#liml <- function(list_X, list_Y, list_Z, list_residuals, n, beta)
liml_os <- liml(data_os[[2]], data_os[[3]], data_os[[1]], data_os[[4]], n, beta)
#jackknife <- function(list_X, list_Y, list_Z, K, n)
jive_os <- jackknife(data_os[[2]], data_os[[3]], data_os[[1]], K, n)

b0_ols_os <- sapply(ols_os, '[[', 1)   
b1_ols_os <- sapply(ols_os, '[[', 2)
b0_tsls_os <- sapply(tsls_os, '[[', 1)   
b1_tsls_os <- sapply(tsls_os, '[[', 2)
b0_liml_os <- sapply(liml_os, '[[', 1)   
b1_liml_os <- sapply(liml_os, '[[', 2)
beta_jive_os <- jive_os[[1]]
b0_jive_os <- sapply(beta_jive_os, '[[', 1)   
b1_jive_os <- sapply(beta_jive_os, '[[', 2)



s1 <- subplot(
  plot_ly(x = b0_ols_os, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_ols_os, y = b1_ols_os, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_ols_os, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_ols_os <- layout(s1, title ="OLS Estimators", showlegend = FALSE)



s2 <- subplot(
  plot_ly(x = b0_tsls_os, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_tsls_os, y = b1_tsls_os, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_tsls_os, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_tsls_os <- layout(s2, title ="2SLS Estimators", showlegend = FALSE)



s3 <- subplot(
  plot_ly(x = b0_liml_os, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_liml_os, y = b1_liml_os, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_liml_os, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_liml_os <- layout(s3, title ="LIML Estimators", showlegend = FALSE)



s4 <- subplot(
  plot_ly(x = b0_jive_os, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_jive_os, y = b1_jive_os, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_jive_os, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_jive_os <- layout(s4, title ="JIVE Estimators", showlegend = FALSE)



p_ols <- qplot(b1_ols_os, geom="histogram", binwidth = 0.02, main = "OLS", xlab = "beta1",  
               fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_tsls <- qplot(b1_tsls_os, geom="histogram", binwidth = 0.02, main = "2SLS", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_liml <- qplot(b1_liml_os, geom="histogram", binwidth = 0.02, main = "LIML", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_jive <- qplot(b1_jive_os, geom="histogram", binwidth = 0.02, main = "JIVE", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))


#save.image(file = "model3.RData")

rm(list=ls())
load(file = "model3.RData")
source("RM_functions.R")

quantile(b1_ols_os, seq(0,1, by = 0.25))
#      0%       25%       50%       75%      100% 
# 0.9541175 1.0444952 1.0628892 1.0811471 1.1609080 
quantile(b1_tsls_os, seq(0,1, by = 0.25))
#      0%       25%       50%       75%      100% 
# 0.9035298 0.9853004 1.0056071 1.0256277 1.1115587 
quantile(b1_liml_os, seq(0,1, by = 0.25))
#      0%       25%       50%       75%      100% 
# 0.8929294 0.9797018 1.0001853 1.0207446 1.1055475  
quantile(b1_jive_os, seq(0,1, by = 0.25))
#      0%       25%       50%       75%      100% 
# 0.8949632 0.9792536 0.9996248 1.0204283 1.1083096 

X_jive_os <- jive_os[[2]]
X <- data_os[[2]]
Y <- data_os[[3]]
#coverage_probability <- function(list_X, list_Y, list_beta, n, r)
cp_ols <- coverage_probability(X, Y, ols_os, n = 200, r = 5000)   # 0.3702
cp_tsls <- coverage_probability(X, Y, tsls_os, n = 200, r = 5000) # 0.9254
cp_liml <- coverage_probability(X, Y, liml_os, n = 200, r = 5000) # 0.9208
cp_jive <- coverage_probability(X_jive_os, Y, beta_jive_os, n = 200, r = 5000) # 0.999

figure <- ggarrange(p_ols, p_tsls, p_liml, p_jive, ncol = 1, nrow = 4)