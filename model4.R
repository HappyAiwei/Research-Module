#########################################################################################################
### Model 4 : Over-identification Weak IV
#########################################################################################################
rm(list=ls())
set.seed(123)
source("RM_functions.R")

## Parameters
K <- 16 # number of regressors of Z 
n <- 200 # number of observations
r <- 5000  # number of repetition
L <- 2 # number of regressors of X
# error terms (epsilon and eta) correlate
sigma <- matrix(c(0.25, 0.2, 0.2, 0.25),2,2)
mu <- c(0, 0)
beta <- c(1, 1)
pi <- c(0, rep(0.1, (K-1)))
list_beta <- rep(list(matrix(data = NA, nrow = L, ncol = 1)), r)

data_ow <- dgp(K, L, n, r, mu, sigma, pi, beta)
#ols <- function(list_X, list_Y)
ols_ow <- ols(data_ow[[2]], data_ow[[3]])
#tsls <- function(list_X, list_Y, list_Z, n)
tsls_ow <- tsls(data_ow[[2]], data_ow[[3]], data_ow[[1]], n)
#liml <- function(list_X, list_Y, list_Z, list_residuals, n, beta)
liml_ow <- liml(data_ow[[2]], data_ow[[3]], data_ow[[1]], data_ow[[4]], n, beta)
#jackknife <- function(list_X, list_Y, list_Z, K, n)
jive_ow <- jackknife(data_ow[[2]], data_ow[[3]], data_ow[[1]], K, n)

b0_ols_ow <- sapply(ols_ow, '[[', 1)   
b1_ols_ow <- sapply(ols_ow, '[[', 2)
b0_tsls_ow <- sapply(tsls_ow, '[[', 1)   
b1_tsls_ow <- sapply(tsls_ow, '[[', 2)
b0_liml_ow <- sapply(liml_ow, '[[', 1)   
b1_liml_ow <- sapply(liml_ow, '[[', 2)
beta_jive_ow <- jive_ow[[1]]
b0_jive_ow <- sapply(beta_jive_ow, '[[', 1)   
b1_jive_ow <- sapply(beta_jive_ow, '[[', 2)


s1 <- subplot(
  plot_ly(x = b0_ols_ow, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_ols_ow, y = b1_ols_ow, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_ols_ow, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_ols_ow <- layout(s1, title ="OLS Estimators", showlegend = FALSE)



s2 <- subplot(
  plot_ly(x = b0_tsls_ow, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_tsls_ow, y = b1_tsls_ow, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_tsls_ow, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_tsls_ow <- layout(s2, title ="2SLS Estimators", showlegend = FALSE)



s3 <- subplot(
  plot_ly(x = b0_liml_ow, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_liml_ow, y = b1_liml_ow, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_liml_ow, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_liml_ow <- layout(s3, title ="LIML Estimators", showlegend = FALSE)



s4 <- subplot(
  plot_ly(x = b0_jive_ow, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_jive_ow, y = b1_jive_ow, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_jive_ow, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_jive_ow <- layout(s4, title ="JIVE Estimators", showlegend = FALSE)


p_ols <- qplot(b1_ols_ow, geom="histogram", binwidth = 0.02, main = "OLS", xlab = "beta1",  
               fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_tsls <- qplot(b1_tsls_ow, geom="histogram", binwidth = 0.02, main = "2SLS", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_liml <- qplot(b1_liml_ow, geom="histogram", binwidth = 0.02, main = "LIML", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))

p_jive <- qplot(b1_jive_ow, geom="histogram", binwidth = 0.02, main = "JIVE", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(0,2))


#save.image(file = "model4.RData")

rm(list=ls())
load(file = "model4.RData")
source("RM_functions.R")

quantile(b1_ols_ow, seq(0,1, by = 0.25))
#     0%      25%      50%      75%     100% 
# 1.334673 1.470346 1.499555 1.529538 1.656621 
quantile(b1_tsls_ow, seq(0,1, by = 0.25))
#     0%       25%       50%       75%      100% 
# 0.7572723 1.0315144 1.0848827 1.1351861 1.3164206 
quantile(b1_liml_ow, seq(0,1, by = 0.25))
#     0%       25%       50%       75%      100% 
# 0.3878991 0.9324686 0.9994824 1.0582838 1.2366573  
quantile(b1_jive_ow, seq(0,1, by = 0.25))
#     0%       25%       50%       75%      100% 
# 0.3875903 0.9150165 0.9861466 1.0522212 1.2798611 

X_jive_ow <- jive_ow[[2]]
X <- data_ow[[2]]
Y <- data_ow[[3]]
#coverage_probability <- function(list_X, list_Y, list_beta, n, r)
cp_ols <- coverage_probability(X, Y, ols_ow, n = 200, r = 5000)   # 0
cp_tsls <- coverage_probability(X, Y, tsls_ow, n = 200, r = 5000) # 0.477
cp_liml <- coverage_probability(X, Y, liml_ow, n = 200, r = 5000) # 0.645
cp_jive <- coverage_probability(X_jive_ow, Y, beta_jive_ow, n = 200, r = 5000) # 0.9962 

figure <- ggarrange(p_ols, p_tsls, p_liml, p_jive, ncol = 1, nrow = 4)