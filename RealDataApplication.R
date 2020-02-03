library("AER"); library("sandwich"); library("lmtest"); library("tidyverse"); library("broom"); library("ivmodel");
library("AER"); library("SteinIV"); library("lmtest"); library("sandwich"); library("ggplot2"); library("scales");
library("readstata13"); library("fastDummies")


rm(list = ls())
data1 <- read.dta13("AK91.dta")
ak91.1 <- mutate(data1,
               q4 = as.integer(qob == "4"),
               q3 = as.integer(qob == "3"),
               q2 = as.integer(qob == "2"))

yobdummies.matrix <- model.matrix(~0+factor(data1$yob))
pobdummies.matrix <- model.matrix(~0+factor(data1$pob))
qobdummies.matrix <- as.matrix(ak91.1[,c("q4", "q3", "q2")])
# data1$yobdummies <- yobdummies.matrix
# data1$pobdummies <- pobdummies.matrix
df <- data.frame(ak91.1[,c("q4", "q3", "q2")], yobdummies.matrix, pobdummies.matrix)

A <- as.matrix(df)
for (i in 1:3) {for (j in 4:13) {A <- cbind(A, A[,i] * A[,j])}}
qob.int.yob <- A[, 65:94]

B <- as.matrix(df)
for (i in 1:3) {for (j in 14:63) {B <- cbind(B, B[,i] * B[,j])}}
qob.int.pob <- B[, 65:214]

allint <- cbind(qob.int.yob, qob.int.pob)
exo.covariates <- cbind(yobdummies.matrix[,2:10], pobdummies.matrix[,2:51])
lnwage <- ak91.1$lnwage
educ <- ak91.1$educ

### control variables: yob    exclued IVs: qob
IV1 <- ivmodel(Y=lnwage, D=educ, Z=qobdummies.matrix, X=yobdummies.matrix[,2:10])
summary(IV1)

### control variables: yob    exclued IVs: qob*yob
IV2 <- ivmodel(Y=lnwage, D=educ, Z=qob.int.yob, X=yobdummies.matrix[,2:10])
summary(IV2)

### control variables: yob, pob  exclued IVs: qob*yob, qob*pob
IV3 <- ivmodel(Y=lnwage, D=educ, Z=allint, X=exo.covariates)
summary(IV3)


########################################################################################################
########################################################################################################
### Jack knife

### control variables: yob    exclued IVs: qob
X1 <- cbind(educ,yobdummies.matrix[,2:10])
Z1 <- cbind(yobdummies.matrix[,2:10], qobdummies.matrix)
JIVE1 <- jive.est(lnwage, X1, Z1, SE=TRUE)
print(JIVE1)

### control variables: yob    exclued IVs: qob*yob
X2 <-X1
Z2 <- cbind(yobdummies.matrix[,2:10], qob.int.yob)
JIVE2 <- jive.est(lnwage, X2, Z2, SE=TRUE)
print(JIVE2)

### control variables: yob, pob  exclued IVs: qob*yob, qob*pob
X3 <- cbind(educ, yobdummies.matrix[,2:10], pobdummies.matrix[,2:51])
Z3 <- cbind(yobdummies.matrix[,2:10], pobdummies.matrix[,2:51], qob.int.yob, qob.int.pob)
JIVE3 <- jive.est(lnwage, X3, Z3, SE=TRUE)
print(JIVE3)
