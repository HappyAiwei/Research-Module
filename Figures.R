##-------------------------------------------------------------------------------
## This code is written by Aiwei Huang.                                         -
## Licensed under The Apache License Version 2.0 [see LICENSE for details]      -
##-------------------------------------------------------------------------------


rm(list=ls())
set.seed(100)
source("RM_functions.R")

#########################################################################################################
### Figure 1 (x-axis:rho, y-axis:Median Bias) 
#########################################################################################################
# here, ||pi||^2 = 0.1 , Var(eta) = 0.9

data_25 <- data_f1(n = obs[1])
data_50 <- data_f1(n = obs[2])
data_100 <- data_f1(n = obs[3])
data_200 <- data_f1(n = obs[4])
data_400 <- data_f1(n = obs[5])
data_800 <- data_f1(n = obs[6])

f1_rho <- seq(from = 0, to = 1, by = 0.01)

mb25 <- MedianBias(data = data_25, n = obs[1], variate = f1_rho, K = 7) # list(mb_ols, mb_2sls, mb_liml, mb_jive)
ols25 <- mb25[[1]]
tsls25 <- mb25[[2]]
liml25 <- mb25[[3]]
jive25 <- mb25[[4]]

mb50 <- MedianBias(data = data_50, n = obs[2], variate = f1_rho, K = 7)
ols50 <- mb50[[1]]
tsls50 <- mb50[[2]]
liml50 <- mb50[[3]]
jive50 <- mb50[[4]]

mb100 <- MedianBias(data = data_100, n = obs[3], variate = f1_rho, K = 7)
ols100 <- mb100[[1]]
tsls100 <- mb100[[2]]
liml100 <- mb100[[3]]
jive100 <- mb100[[4]]

mb200 <- MedianBias(data = data_200, n = obs[4], variate = f1_rho, K = 7)
ols200 <- mb200[[1]]
tsls200 <- mb200[[2]]
liml200 <- mb200[[3]]
jive200 <- mb200[[4]]

mb400 <- MedianBias(data = data_400, n = obs[5], variate = f1_rho, K = 7)
ols400 <- mb400[[1]]
tsls400 <- mb400[[2]]
liml400 <- mb400[[3]]
jive400 <- mb400[[4]]

mb800 <- MedianBias(data = data_800, n = obs[6], variate = f1_rho, K = 7)
ols800 <- mb800[[1]]
tsls800 <- mb800[[2]]
liml800 <- mb800[[3]]
jive800 <- mb800[[4]]

df <- data.frame(f1_rho, ols25, tsls25, liml25, jive25, ols50, tsls50, liml50, jive50, 
                       ols100, tsls100, liml100, jive100,ols200, tsls200, liml200, jive200,
                       ols400, tsls400, liml400, jive400,ols800, tsls800, liml800, jive800)

write.csv(df, file = "MedianBias1.csv", row.names = FALSE)


#########################################################################################################
### Figure 2 (x-axis:R^2, y-axis:Median Bias) 
#########################################################################################################
# here, R^2 = 1 / [1 + (Var(eta) / ||pi||^2)]   and   Var(eta) + ||pi||^2 = 1
# we denote R^2 as a, then ||pi||^2 = a, Var(eta) = 1 - a, pi1,...,pi6 = sqrt(a/6)

rm(list=ls())
set.seed(100)
source("RM_functions.R")

data_25 <- data_f2(n = obs[1]) 
data_50 <- data_f2(n = obs[2])
data_100 <- data_f2(n = obs[3])
data_200 <- data_f2(n = obs[4])
data_400 <- data_f2(n = obs[5])
data_800 <- data_f2(n = obs[6])

a <- seq(from = 0.01, to = 1, by = 0.01)

mb25 <- MedianBias(data = data_25, n = obs[1], variate = a, K = 7)
ols25 <- mb25[[1]]
tsls25 <- mb25[[2]]
liml25 <- mb25[[3]]
jive25 <- mb25[[4]]

mb50 <- MedianBias(data = data_50, n = obs[2], variate = a, K = 7)
ols50 <- mb50[[1]]
tsls50 <- mb50[[2]]
liml50 <- mb50[[3]]
jive50 <- mb50[[4]]

mb100 <- MedianBias(data = data_100, n = obs[3], variate = a, K = 7)
ols100 <- mb100[[1]]
tsls100 <- mb100[[2]]
liml100 <- mb100[[3]]
jive100 <- mb100[[4]]

mb200 <- MedianBias(data = data_200, n = obs[4], variate = a, K = 7)
ols200 <- mb200[[1]]
tsls200 <- mb200[[2]]
liml200 <- mb200[[3]]
jive200 <- mb200[[4]]

mb400 <- MedianBias(data = data_400, n = obs[5], variate = a, K = 7)
ols400 <- mb400[[1]]
tsls400 <- mb400[[2]]
liml400 <- mb400[[3]]
jive400 <- mb400[[4]]

mb800 <- MedianBias(data = data_800, n = obs[6], variate = a, K = 7)
ols800 <- mb800[[1]]
tsls800 <- mb800[[2]]
liml800 <- mb800[[3]]
jive800 <- mb800[[4]]

df <- data.frame(a, ols25, tsls25, liml25, jive25, ols50, tsls50, liml50, jive50, 
                       ols100, tsls100, liml100, jive100,ols200, tsls200, liml200, jive200,
                       ols400, tsls400, liml400, jive400,ols800, tsls800, liml800, jive800)

write.csv(df, file = "MedianBias2.csv", row.names = FALSE)

#########################################################################################################
### Figure 3 (x-axis:K - 2, y-axis:Median Bias) 
#########################################################################################################
# here, R^2 = 0.1 , ||pi||^2 = 0.1 , Var(eta) = 0.9 , f3_rho <- 0.9 

rm(list=ls())
set.seed(100)
source("RM_functions.R")

data_25 <- data_f3(obs[1])
data_50 <- data_f3(n = obs[2])
data_100 <- data_f3(n = obs[3])
data_200 <- data_f3(n = obs[4])
data_400 <- data_f3(n = obs[5])
data_800 <- data_f3(n = obs[6])

K3 <- seq(from = 0, to = 16, by = 1) + L

mb25 <- MedianBias_f3(data = data_25, n = obs[1], variate = K3, K = K3)
ols25 <- mb25[[1]]
tsls25 <- mb25[[2]]
liml25 <- mb25[[3]]
jive25 <- mb25[[4]]

mb50 <- MedianBias_f3(data = data_50, n = obs[2], variate = K3, K = K3)
ols50 <- mb50[[1]]
tsls50 <- mb50[[2]]
liml50 <- mb50[[3]]
jive50 <- mb50[[4]]

mb100 <- MedianBias_f3(data = data_100, n = obs[3], variate = K3, K = K3)
ols100 <- mb100[[1]]
tsls100 <- mb100[[2]]
liml100 <- mb100[[3]]
jive100 <- mb100[[4]]

mb200 <- MedianBias_f3(data = data_200, n = obs[4], variate = K3, K = K3)
ols200 <- mb200[[1]]
tsls200 <- mb200[[2]]
liml200 <- mb200[[3]]
jive200 <- mb200[[4]]

mb400 <- MedianBias_f3(data = data_400, n = obs[5], variate = K3, K = K3)
ols400 <- mb400[[1]]
tsls400 <- mb400[[2]]
liml400 <- mb400[[3]]
jive400 <- mb400[[4]]

mb800 <- MedianBias_f3(data = data_800, n = obs[6], variate = K3, K = K3)
ols800 <- mb800[[1]]
tsls800 <- mb800[[2]]
liml800 <- mb800[[3]]
jive800 <- mb800[[4]]

df <- data.frame(K3, ols25, tsls25, liml25, jive25, ols50, tsls50, liml50, jive50, 
                       ols100, tsls100, liml100, jive100,ols200, tsls200, liml200, jive200,
                       ols400, tsls400, liml400, jive400,ols800, tsls800, liml800, jive800)

write.csv(df, file = "MedianBias3.csv", row.names = FALSE)


###########################################################################################################
###########################################################################################################

#mb1 <- read.csv(file = "MedianBias1.csv", header = TRUE, sep = ",")
#mb2 <- read.csv(file = "MedianBias2.csv", header = TRUE, sep = ",")
#mb3 <- read.csv(file = "MedianBias3.csv", header = TRUE, sep = ",")

