##-------------------------------------------------------------------------------
## This code is written by Aiwei Huang.                                         -
## Licensed under The Apache License Version 2.0 [see LICENSE for details]      -
##-------------------------------------------------------------------------------

rm(list=ls())
set.seed(123)
source("RM_functions.R")

#########################################################################################################
### Model 2 : Just-identification Weak IV
#########################################################################################################

## Parameters
K <- 2 # number of regressors of Z 
n <- 200 # number of observations
r <- 5000  # number of repetition
L <- 2 # number of regressors of X
# error terms (epsilon and eta) correlate
sigma = matrix(c(1, 0.9, 0.9, 1),2,2)
mu <- c(0, 0)
beta <- c(1, 1)
pi <- c(0, 0.2)
list_beta <- rep(list(matrix(data = NA, nrow = L, ncol = 1)), r)

data_jw <- dgp(K, L, n, r, mu, sigma, pi, beta)
#ols <- function(list_X, list_Y)
ols_jw <- ols(data_jw[[2]], data_jw[[3]])
#tsls <- function(list_X, list_Y, list_Z, n)
tsls_jw <- tsls(data_jw[[2]], data_jw[[3]], data_jw[[1]], n)
#liml <- function(list_X, list_Y, list_Z, list_residuals, n, beta)
liml_jw <- liml(data_jw[[2]], data_jw[[3]], data_jw[[1]], data_jw[[4]], n, beta)
#jackknife <- function(list_X, list_Y, list_Z, K, n)
jive_jw <- jackknife(data_jw[[2]], data_jw[[3]], data_jw[[1]], K, n)
beta_jive_jw <- jive_jw[[1]]

b0_ols_jw <- sapply(ols_jw, '[[', 1)   
b1_ols_jw <- sapply(ols_jw, '[[', 2)
b0_tsls_jw <- sapply(tsls_jw, '[[', 1)   
b1_tsls_jw <- sapply(tsls_jw, '[[', 2)
b0_liml_jw <- sapply(liml_jw, '[[', 1)   
b1_liml_jw <- sapply(liml_jw, '[[', 2)
beta_jive_jw <- jive_jw[[1]]
b0_jive_jw <- sapply(beta_jive_jw, '[[', 1)   
b1_jive_jw <- sapply(beta_jive_jw, '[[', 2)



s1 <- subplot(
  plot_ly(x = b0_ols_jw, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_ols_jw, y = b1_ols_jw, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_ols_jw, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_ols_jw <- layout(s1, title ="OLS Estimators", showlegend = FALSE)



s2 <- subplot(
  plot_ly(x = b0_tsls_jw, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_tsls_jw, y = b1_tsls_jw, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_tsls_jw, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_tsls_jw <- layout(s2, title ="2SLS Estimators", showlegend = FALSE)



s3 <- subplot(
  plot_ly(x = b0_liml_jw, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_liml_jw, y = b1_liml_jw, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_liml_jw, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_liml_jw <- layout(s3, title ="LIML Estimators", showlegend = FALSE)



s4 <- subplot(
  plot_ly(x = b0_jive_jw, color = I("black"), type = 'histogram'), 
  plotly_empty(), 
  plot_ly(x = b0_jive_jw, y = b1_jive_jw, type = 'histogram2dcontour', showscale = F)%>%
    layout(xaxis = list(title ='beta0'),
           yaxis = list(title ='beta1')), 
  plot_ly(y = b1_jive_jw, color = I("black"), type = 'histogram'),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), 
  shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
p_jive_jw <- layout(s4, title ="JIVE Estimators", showlegend = FALSE)


p_ols <- qplot(b1_ols_jw, geom="histogram", binwidth = 0.01, main = "OLS", xlab = "beta1",  
               fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(-4,5))

p_tsls <- qplot(b1_tsls_jw, geom="histogram", binwidth = 0.1, main = "2SLS", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(-4,5))

p_liml <- qplot(b1_liml_jw, geom="histogram", binwidth = 0.1, main = "LIML", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(-4,5))

p_jive <- qplot(b1_jive_jw, geom="histogram", binwidth = 0.1, main = "JIVE", xlab = "beta1",  
                fill=I("blue"), col=I("red"), alpha=I(.2), xlim=c(-4,5))


#save.image(file = "model2.RData")

rm(list=ls())
load(file = "model2.RData")
source("RM_functions.R")

quantile(b1_ols_jw, seq(0,1, by = 0.25))
#     0%      25%      50%      75%     100% 
# 1.743948 1.842912 1.864088 1.886178 1.975238 
quantile(b1_tsls_jw, seq(0,1, by = 0.25))
#        0%          25%          50%          75%         100% 
# -241.3487299    0.7025783    1.0051639    1.1978083   81.9323479 
quantile(b1_liml_jw, seq(0,1, by = 0.25))
#        0%          25%          50%          75%         100% 
# -403.4926696    0.7703779    1.0588634    1.2118475 1532.5634432 
quantile(b1_jive_jw, seq(0,1, by = 0.25))
#        0%          25%          50%          75%         100% 
# -627.8095135    0.2646095    0.8402243    1.1493856 2183.6029986 

X_jive_jw <- jive_jw[[2]]
X <- data_jw[[2]]
Y <- data_jw[[3]]
#coverage_probability <- function(list_X, list_Y, list_beta, n)
cp_ols <- coverage_probability(X, Y, ols_jw, n = 200, r = 5000)   # 0
cp_tsls <- coverage_probability(X, Y, tsls_jw, n = 200, r = 5000) # 0.1414
cp_liml <- coverage_probability(X, Y, liml_jw, n = 200, r = 5000) # 0.1446
cp_jive <- coverage_probability(X_jive_jw, Y, beta_jive_jw, n = 200, r = 5000) # 0.8464

  
#library(ggpubr)
figure <- ggarrange(p_ols, p_tsls, p_liml, p_jive, ncol = 1, nrow = 4)




