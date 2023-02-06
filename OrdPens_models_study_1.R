###  Using OrdPens for ordinal predictor variables:

setwd("C:/Users/sherr/Desktop/Minor_Thesis/terror_management")
msd <- read.csv("study_1_MS_analysis_spread.csv")
msd <- read.csv("ordPens_indicator_variables.csv")
library(dplyr)
install.packages("ordPens")
library(ordPens)

View(msd)

# Using all individual questions as factors.

x1 <- msd$se1
x2 <- msd$se2
x3 <- msd$se3
x4 <- msd$se4
x5 <- msd$se5
x6 <- msd$se6
x7 <- msd$se7
x8 <- msd$se8
x9 <- msd$se9
x10 <- msd$se10
x11 <- msd$ident1
x12 <- msd$ident2
x13 <- msd$ident3
x14 <-(msd$se2_ind*msd$coded_ms)+1
x15 <- (msd$id3_ind*msd$coded_ms)+1
x16 <- msd$int_111+1
View(x16)
msd$se3_ind[]
# the interaction term with original variables coded in order
# IS NOT SIGNIFICANT: not even borderline. not one simulation included it.


# not good use of the program to with indicator variables as they 
# are just binary

View(msd$orig_int3)
hist(x13, breaks=9)
#  x2 missing 1,2,3 and x12 missing 3


u <- x16#cbind(x16)
u <- matrix(u)
class(u)
x <- cbind(x1,x3,x4,x5,x6,x7,x8,x9,x10,x11,x13)
View(u)

# transform y (normal)
y<-msd$PCA3 #mca12
cor(msd$mlatt3, msd$mlatt4)
uy <- mean(y)
sdy <-sd(y)
std_y <- (y-uy)/sdy
trans_y <- sqrt(std_y+5)
plot(density(y))
shapiro.test(trans_y)
y<- msd$wdefense
trans_y <- 1/(max(y)+1-y)
plot(density(trans_y))

ordAOV(x=x, y=y, type = "RLRT", nsim=1000000)

# ANOVA tested x matrix:

ms <- read.csv("study_1_MS_analysis_spread.csv")
x3 <- ms$se3 #p = 0.02541
x13 <- ms$ident3 # p< 0.0001
View(x13)
x_aov <- cbind(x3,x13)#x14 is se3_ind(with coded_ms)

lambda <- c(500, 400, 200,100,50)
# 10 suits x3 but not as much x13 using ordSelect
model <- ordSelect(x=x_aov, y=trans_y,lambda=lambda) #u=u
lambda <- c(70)#100,90.80,60,40,30)
model<- ordSmooth(x=x_aov, y=trans_y,lambda=lambda)

round(model$coefficients,digits=3) 
plot(model)
class(model$fitted)

residuals <-as.vector(y - model$fitted)
plot(residuals, type='o',main="Residuals for ordPens Model")
abline(h=0, col='red')
class(residuals)
ur <- mean(residuals)
sdr <- sd(residuals)
std_r <-(residuals - ur)/sdr


shapiro.test(std_r)
plot(density(std_r))
plot(model$fitted~y, main="Residuals vs. Independent Variable")
library(car)
ncvTest(model)
??ncvTest
durbinWatsonTest(residuals)
plot(std_r ~ model$fitted, main="ordSmooth Model: Residuals vs. Fitted Values",
     ylab="Standardized Residuals", xlab="Fitted Values")# 
abline(h=0, col="red")
qqnorm(residuals)
qqline(residuals)

AICmanual <- nrow(msd)*(log(2*pi)+1+log((sum(residuals^2)/nrow(msd))))+((length(model$coefficients)+1)*2)
AICmanual
residuals
int <- msd$se2*x13*msd$coded_ms
plot(y~int)
# -----------------------------------------------

# Correlations between individual questions and PCAs: sePC1, sePC2, sePC3, idPC1

cor(msd[,c(58:67, 2)])# for self esteem questiosn
cor(msd[,c(68:70, 3)])# for identity questions
