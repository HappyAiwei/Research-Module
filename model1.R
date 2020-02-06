##-------------------------------------------------------------------------------
## This code is written by Aiwei Huang.                                         -
## Licensed under The Apache License Version 2.0 [see LICENSE for details]      -
##-------------------------------------------------------------------------------

rm(list=ls())
set.seed(123)
source("RM_functions.R")

#########################################################################################################
### Model 1 : Just-identification Strong IV
#########################################################################################################
## Parameters
K <- 2 # number of regressors of Z 
n <- 200 # number of observations
r <- 5000  # number of repetition
L <- 2 # number of regressors of X
# error terms (epsilon and eta) correlate 
sigma = matrix(c(0.25, 0.2, 0.2, 0.25),2,2)
mu <- c(0, 0)
beta <- c(1, 1)
pi <- c(0, 0.3)
list_beta <- rep(list(matrix(data = NA, nrow = L, ncol = 1)), r)

data_js <- dgp(K, L, n, r, mu, sigma, pi, beta)
#ols <- function(list_X, list_Y)
ols_js <- ols(data_js[[2]], data_js[[3]])
#tsls <- function(list_X, list_Y, list_Z, n)
tsls_js <- tsls(data_js[[2]], data_js[[3]], data_js[[1]], n)
#liml <- function(list_X, list_Y, list_Z, list_residuals, n, beta)
liml_js <- liml(data_js[[2]], data_js[[3]], data_js[[1]], data_js[[4]], n, beta)
#jackknife <- function(list_X, list_Y, list_Z, K, n)
jive_js <- jackknife(data_js[[2]], data_js[[3]], data_js[[1]], K, n)


b0_ols_js <- sapply(ols_js, '[[', 1)   
b1_ols_js <- sapply(ols_js, '[[', 2)
b0_tsls_js <- sapply(tsls_js, '[[', 1)   
b1_tsls_js <- sapply(tsls_js, '[[', 2)
b0_liml_js <- sapply(liml_js, '[[', 1)   
b1_liml_js <- sapply(liml_js, '[[', 2)
beta_jive_js <- jive_js[[1]]
b0_jive_js <- sapply(beta_jive_js, '[[', 1)   
b1_jive_js <- sapply(beta_jive_js, '[[', 2)


s1 <- subplot(
  plot_ly(x = b0_ols_js, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_ols_js, y = b1_ols_js, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_ols_js, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_ols_js <- layout(s1, title ="OLS Estimators", showlegend = FALSE)




s2 <- subplot(
  plot_ly(x = b0_tsls_js, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_tsls_js, y = b1_tsls_js, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_tsls_js, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_tsls_js <- layout(s2, title ="2SLS Estimators", showlegend = FALSE)




s3 <- subplot(
  plot_ly(x = b0_liml_js, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_liml_js, y = b1_liml_js, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_liml_js, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_liml_js <- layout(s3, title ="LIML Estimators", showlegend = FALSE)





s4 <- subplot(
  plot_ly(x = b0_jive_js, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_jive_js, y = b1_jive_js, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_jive_js, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_jive_js <- layout(s4, title ="JIVE Estimators", showlegend = FALSE)



p_ols <- qplot(b1_ols_js, geom="histogram", binwidth = 0.02, main = "OLS", xlab = "beta1",  
               fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_tsls <- qplot(b1_tsls_js, geom="histogram", binwidth = 0.02, main = "2SLS", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_liml <- qplot(b1_liml_js, geom="histogram", binwidth = 0.02, main = "LIML", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_jive <- qplot(b1_jive_js, geom="histogram", binwidth = 0.02, main = "JIVE", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))


#save.image(file = "model1.RData")

rm(list=ls())
load(file = "model1.RData")
source("RM_functions.R")

quantile(b1_ols_js, seq(0,1, by = 0.25))
#     0%      25%      50%      75%     100% 
# 1.430346 1.558159 1.587495 1.617640 1.751229 
quantile(b1_tsls_js, seq(0,1, by = 0.25))
#     0%       25%       50%       75%      100% 
# 0.4173871 0.9152048 1.0000021 1.0737110 1.3707700 
quantile(b1_liml_js, seq(0,1, by = 0.25))
#     0%        25%        50%        75%       100% 
# -0.3882026  0.9088549  1.0000070  1.0696983  1.3080130 
quantile(b1_jive_js, seq(0,1, by = 0.25))
#     0%       25%       50%       75%      100% 
# 0.2577784 0.8862220 0.9774028 1.0555076 1.3591492 

X_jive_js <- jive_js[[2]]
X <- data_js[[2]]
Y <- data_js[[3]]
#coverage_probability <- function(list_X, list_Y, list_beta, n)
cp_ols <- coverage_probability(X, Y, ols_js, n = 200, r = 5000)   # 0
cp_tsls <- coverage_probability(X, Y, tsls_js, n = 200, r = 5000) # 0.536
cp_liml <- coverage_probability(X, Y, liml_js, n = 200, r = 5000) # 0.5386
cp_jive <- coverage_probability(X_jive_js, Y, beta_jive_js, n = 200, r = 5000) # 0.9998

figure <- ggarrange(p_ols, p_tsls, p_liml, p_jive, ncol = 1, nrow = 4)