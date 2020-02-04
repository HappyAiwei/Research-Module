library("AER"); library("sandwich"); library("lmtest"); library("tidyverse"); library("broom"); library("ivmodel");
library("AER"); library("SteinIV"); library("lmtest"); library("sandwich"); library("ggplot2"); library("scales");
library("readstata13"); library("fastDummies")


rm(list = ls())
data1 <- read.dta13("AK91.dta")
ak91.1 <- mutate(data1,
                 q4 = as.integer(qob == "4"),
                 q3 = as.integer(qob == "3"),
                 q2 = as.integer(qob == "2"))
lnwage <- ak91.1$lnwage
educ <- ak91.1$educ

###average wages by age
ak91.2 <- ak91.1 %>%        
  group_by(qob, yob) %>%
  summarise(lnwage = mean(lnwage), educ = mean(educ)) %>%
  mutate(q4 = (qob == 4))

### 1st stage plot
plot1 <- ggplot(ak91.2, aes(x = yob + (qob - 1) / 4, y = educ)) +
  geom_line() +
  geom_label(mapping = aes(label = qob, color = q4, size = 0)) +
  theme(legend.position = "none") +
  scale_x_continuous("Year of birth", breaks = seq(30,39, by = 1), limits = c(30, 40)) +
  scale_y_continuous("Years of Education", breaks = seq(12.2, 13.2, by = 0.2),
                     limits = c(12.2, 13.2))

### 2nd stage plot
plot2 <- ggplot(ak91.2, aes(x = yob + (qob - 1) / 4, y = lnwage)) +
  geom_line() +
  geom_label(mapping = aes(label = qob, color = q4)) +
  scale_x_continuous("Year of birth", breaks = seq(30,39, by = 1), limits = c(30, 40)) +
  scale_y_continuous("Log weekly wages") +
  theme(legend.position = "none")

###consruction of dummy variables
yobdummies.matrix <- model.matrix(~0+factor(data1$yob))
pobdummies.matrix <- model.matrix(~0+factor(data1$pob))
qobdummies.matrix <- as.matrix(ak91.1[,c("q4", "q3", "q2")])
df <- data.frame(ak91.1[,c("q4", "q3", "q2")], yobdummies.matrix, pobdummies.matrix)

###1st stage estimations
mod1 <- lm(educ ~ q4, data = ak91.1)
coeftest(mod1, vcov = sandwich)
mod2 <- lm(educ ~ qobdummies.matrix)
coeftest(mod2, vcov = sandwich)
mod3 <- lm(educ ~ q4 + yobdummies.matrix[,2:10], data = ak91.1)
coeftest(mod3, vcov = sandwich)
mod4 <- lm(educ ~ qobdummies.matrix + yobdummies.matrix[,2:10])
coeftest(mod4, vcov = sandwich)
mod5 <- lm(educ ~ qobdummies.matrix + pobdummies.matrix[,2:51])
coeftest(mod5, vcov = sandwich)

### construction of interaction terms 
A <- as.matrix(df)
for (i in 1:3) {for (j in 4:13) {A <- cbind(A, A[,i] * A[,j])}}
qob.int.yob <- A[, 65:94]

B <- as.matrix(df)
for (i in 1:3) {for (j in 14:63) {B <- cbind(B, B[,i] * B[,j])}}
qob.int.pob <- B[, 65:214]

allint <- cbind(qob.int.yob, qob.int.pob)
exo.covariates <- cbind(yobdummies.matrix[,2:10], pobdummies.matrix[,2:51])

############################# TSLS and LIML ##########################

### control variables: yob    exclued IVs: qob
IV1 <- ivmodel(Y=lnwage, D=educ, Z=qobdummies.matrix, X=yobdummies.matrix[,2:10])
summary(IV1)

### control variables: yob    exclued IVs: qob*yob
IV2 <- ivmodel(Y=lnwage, D=educ, Z=qob.int.yob, X=yobdummies.matrix[,2:10])
summary(IV2)

### control variables: yob, pob  exclued IVs: qob*yob, qob*pob
IV3 <- ivmodel(Y=lnwage, D=educ, Z=allint, X=exo.covariates)
summary(IV3)


############################ Jack knife ##############################

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
