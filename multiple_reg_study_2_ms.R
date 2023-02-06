## Minor Thesis - rehash- Terror Management Theory in a Multicultural Society
# Zwarte Piet study

setwd("C:/Users/sherr/Desktop/Minor_Thesis/terror_management")
msp <- read.csv("study_2_MS_analysis_spread.csv")
library(dplyr)
msp <-rename(msp,piet=ï..piet)
View(msp)
msp <- msp[-c(170),]
class(ms)
# for some reason read.csv is creating two rows of NULL values
# read.table doesn't 174 rows read only
ms <- read.table("study_2_MS_analysis_spread.csv", sep=",", header=TRUE)
View(test)

#   -----------------------------------------------------------
#   -----------------------------------------------------------
x1 <- msp$se1_ind
x2 <- msp$se2_ind
x3 <- msp$se3_ind
x4 <- msp$se4_ind
x5 <- msp$se5_ind
x6 <- msp$se6_ind
x7 <- msp$se7_ind
x8 <- msp$se8_ind
x9 <- msp$se9_ind
x10 <- msp$se10_ind
x11 <- msp$id1_ind
x12 <- msp$id2_ind
x13 <- msp$id3_ind
x14 <- msp$coded_ms
levels(x12)

x1 <- msp$coded_ms
x2 <- msp$sePC1
x3 <- msp$sePC2
x4 <- msp$sePC3
x5 <- msp$idPC1
se <- msp$Zsemean
id <- msp$Zident
ms <- msp$coded_ms
msXse <- ms*se
seXid <- se*id
msXid <- ms*id
msXseXid <- ms*se*id
y <- msp$pietPCPC
z<- msp$piet
hist(y, breaks=15)
plot(density(z), main="Density Plot for Response Variable: piet")
# MULTIPLE REGRESSION:
??glm

trans_y <- 1/(y)
plot(density(trans_y), main="Density Plot for Response Variable: 1/piet")
hist(trans_y, breaks=15)
shapiro.test(trans_y)
#gamma family can use the links "inverse", "identity", "log"

gamma_fit<-glm(trans_y~factor(x1)+factor(x2)+factor(x3)+
                 factor(x4)+factor(x5)+factor(x6)+factor(x7)+factor(x8)+
                 factor(x9)+factor(x10)+factor(x11)+factor(x12)+factor(x13)+
                 factor(x14),family=Gamma(link="identity"), data=msp)#factor(ms):se:id

gamma_fit<-glm(trans_y~factor(x3)+
                 factor(x8)+
                 factor(x12),
                family=Gamma(link="identity"), data=msp)
summary(gamma_fit)
car::vif(gamma_fit)
###########################################################3
new_matrix <- cbind(msp, seXid, msXid, msXse, msXseXid)
cor(new_matrix[,c(3,4,5,42:45)])

pchisq(gamma_fit$deviance, df=gamma_fit$df.residual, lower.tail=FALSE)
# gets 1 is that possible?  If so, then good fit
??pchisq

confint.default(gamma_fit)
dev_res <- residuals(gamma_fit, type='deviance') # type= pearson or deviance
plot(dev_res, type='o')# autocorrelation
abline(h=0, col="red")
plot(dev_res ~ gamma_fit$fitted.values)# looks like some variance
qqnorm(dev_res)
qqline(dev_res)
hist(dev_res, breaks=15)
shapiro.test(dev_res)#normality test
library(car)
ncvTest(gamma_fit)# test of variance

car::vif(gamma_fit)
par(mfrow=c(2,2))
plot(gamma_fit, which=1)
plot(gamma_fit, which=2)
plot(gamma_fit, which=3)
plot(gamma_fit, which=5)
par(mfrow=c(1,1))
plot(gamma_fit, which = 4, id.n = 6)
install.packages('boot')
library(boot)
glm.diag.plots(gamma_fit, glmdiag = glm.diag(gamma_fit))

anova(gamma_fit, complex_model, test="Chi")# test of sig difference between model

# ----------------------------------------------
#   From textbook: Analysis of residuals for gamma:
print(influence.measures(gamma_fit))

yhat <- gamma_fit$fitted.values
plot(yhat, dev_res)
plot(msp$se3, dev_res)
plot(msp$se10, dev_res)
plot(msp$ident3, dev_res)
#  ________________________________________________

#   Hosmer and Lemeshow goodness of fit (GOF) test
-------------------------------------------------------
install.packages("ResourceSelection")
??ResourceSelection
library(ResourceSelection)
#hoslem.test(data$y, fitted(model))
hoslem.test(msp$pietPCPC, fitted(gamma_fit))
plot(fitted(gamma_fit)~msp$pietPCPC)
cor(fitted(gamma_fit),msp$pietPCPC)
-------------------------------------------------------
par(mfrow=c(2,2))
plot(gamma_fit, which=1)
plot(gamma_fit, which=2)
plot(gamma_fit, which=3)
plot(gamma_fit, which=4)
par(mfrow=c(1,1))


# variance constant: FAIL to reject null hypothesis
hist(residuals(ms_orig))

# no multicollinearity as all under 1.002
plot(ms[,c(1,2,3,4)])
cor(ms[,c(2,3,4,5,6)])
# does not appear to be multicollinearity

#   -------------------------------------------------------

#  PCA analysis using PRINCALS in r
#install.packages("Gifi", repos="http://R-Forge.R-project.org")
library(Gifi)
??Gifi

se101 <- msp[,18:27]
piet8 <-msp[,28:35]
id3_ <- msp[,35:37]
pietPC2 <- msp[,9:10]
View(piet8)

## -----------------------------------------------
# test for multivariate normality and outliers:
install.packages("mvnormtest")
library(mvnormtest)
# 1. transform data.frame into matrix:
normtest <- data.matrix(se10)
class(normtest) # matrix
View(normtest)
attributes(normtest)
# 2. use t() to transpose matrix
normtest <- t(normtest)
class(normtest) # data frame
??mshapiro.test
# 3. Perform multivariate normal test:
mshapiro.test(normtest)
# finally mshapiro.test works.  but the data is not multivariate normal
# p value <0.000

# MVN package for a different type of test:
install.packages("MVN")
library(MVN)

# -----------------------------------------------------------------
## ordinal PCA
fitord <- princals(se101, ndim=1)  ## ordinal PCA
fitord
summary(fitord)
plot(fitord, plot.type = "transplot")
plot(fitord, "loadplot", main = "Loadings Plot se10 Data")  ## aspect ratio = 1
plot(fitord, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord, "screeplot")

fitord_p <- princals(piet8, ndim=1)  ## ordinal PCA
fitord_p
summary(fitord_p)
plot(fitord_p, plot.type = "transplot")
plot(fitord_p, "loadplot", main = "Loadings Plot piet8 Data")  ## aspect ratio = 1
plot(fitord_p, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_p, "screeplot")

fitord_i <- princals(id3_, ndim=1)  ## ordinal PCA
fitord_i
summary(fitord_i)
plot(fitord_i, plot.type = "transplot")
plot(fitord_i, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_i, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_i, "screeplot")

fitord_piets <- princals(pietPC2, ndim=1)  ## ordinal PCA
fitord_piets
summary(fitord_piets)
plot(fitord_piets, plot.type = "transplot")
plot(fitord_piets, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_piets, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_piets, "screeplot")

#  -------------------------------------------------
#  --------------------------------------------------

# Multiple regression with PCs:
avePietTrans <- sqrt(ms$avePiet)
View(avePietTrans)
pc_reg<-lm(avePietTrans ~ sePC1 +sePC2 + sePC3 + factor(ms)+ idPC1 +
                sePC1:sePC2 + sePC1:factor(ms) + sePC1:idPC1 +
                sePC1:sePC3 + sePC2:factor(ms) + sePC2:idPC1 +
                sePC2:sePC3 + factor(ms):idPC1 + factor(ms):sePC3 +
                idPC1:sePC3, data=ms)
pc_reg<-lm(avePietTrans ~ sePC1 +sePC2 + sePC3 + factor(ms)+ idPC1, data=ms)

pc_reg<-lm(avePietTrans ~ idPC1, data=ms)

#  --------------------------------------------------

summary(pc_reg)
summary(pcpc_reg)
confint.default(pc_reg)
anova(pc_reg)
library(car)
Anova(pc_reg, type="III")
?Anova

# best model:
library(MASS)
stepReg=MASS::stepAIC(pc_reg, direction="both")
# best model only has intercept and idPC1
stepReg$anova

Anova(pc_reg) #(Anova-not anova-order of variable doesn't matter)# same as summary
anova(pc_reg)
plot(pc_reg)

par(mfrow=c(2,2))
plot(pc_reg, which=1)
plot(pc_reg, which=2)
plot(pc_reg, which=3)
plot(gamma_fit, which=4)
par(mfrow=c(1,1))

res_std=scale(residuals(pc_reg))
plot(res_std ~ pc_reg$fitted.values)
#
shapiro.test(residuals(pc_reg))
# not normally distributed with all interaction terms: null hypothesis rejected
library(car)
durbinWatsonTest(pc_reg)
plot(residuals(pc_reg), type="o")
abline(h=0, col='red')
# errors uncorrelated: FAIL to reject null hypothesis
ncvTest(pc_reg)
# variance constant: FAIL to reject null hypothesis
hist(residuals(pc_reg))

library(ggplot2)
run_color_coded<-data.frame(fitted=ms_linear$fitted.values, residuals=ms_linear$residuals, ms=ms$ms_factor )
res_plot<-ggplot(data=run_color_coded, aes(x=fitted, y=residuals, colour=factor(ms)))
res_plot + geom_point(size=3)
# this shows that two different runs have different slopes

# multcolinearity?
car::vif(pc_reg)
# no multicollinearity as all under 1.002
plot(ms[,c(12,16)]) # show relationship between avePiet & idPC1
cor(ms[,c(12,16)])
cor(ms[,c(4,14)])# strong negative correlation between Zident & idPC1

# does not appear to be multicollinearity

#  --------------------------------------------------
#  --------------------------------------------------
# Boxcox Transform of reponse variable

library(MASS)
y <- ms$pietPCPC
x1 <- ms$pietPC1
x2 <- ms$pietPC2
x3 <- ms$sePC1
x4 <- ms$sePC2
x5 <- ms$ms

bc <- boxcox(y~x1 + x2 + x3 + x4 + x5) ### THIS WORKS WITH SAME PLOT AS BOXCOX.ar !!!
best_lambda <- bc$x[which.max(bc$y)]
round(best_lambda,1) ## i
# can't do log with neg values
min(ms$y)

sq_y <- (y)^2
# best lambda is 1.6 for y_nonneg
lambda = 1.6

y_boxcox= ((y_nonneg^lambda)-1)/lambda
boxcox_fit <- lm(y_boxcox ~ x1 +x2)
summary(boxcox_fit)

shapiro.test(residuals(boxcox_fit)) 
shapiro.test(log_y)
hist(sq_y)

#  -------------------------------------------------------
#  -------------------------------------------------------
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
x14 <- msd$ms
View(x1)
y <- msd$piet
factor14 <- glm(pietPCPC ~ x1 + x2 + x3 + x4 + x5 + 
                  x6 + x7 + x8 + x9 + x10 + 
                  x11 + x12 + x13 + x14, family=Gamma(link="log"),data=msp)
??lm()
summary(factor14)
# after model selection: 
factor14 <- glm(pietPCPC ~ x4  + x13 + x14, family=Gamma(link="log"), data=msp)
summary(factor14)
# x4, x11, x12 for pietPCPC
# x12 piet
# factors: x4, x9, x13, x14 pietPCPC with factorized independents

library(MASS)
stepReg=MASS::stepAIC(factor14, direction="both")
stepReg$anova


shapiro.test(residuals(factor14))
library(car)
ncvTest(factor14)
durbinWatsonTest(factor14)
hist(residuals(factor14), main="Historgram of Residuals: variable pietPCPC")
hist(msd$mlattPCPC1)
car::vif(factor14)

par(mfrow=c(2,2))
plot(factor14, which=1)
plot(factor14, which=2)
plot(factor14, which=3)
plot(factor14, which=5)
par(mfrow=c(1,1))

#  --------------------------------------
#  ---------------------------------------

# Residual checks for glm() regression:
dev_res <- residuals(factor14, type='deviance')
plot(dev_res, type='o')# autocorrelation
abline(h=0, col="red")
??abline
plot(dev_res ~ factor14$fitted.values)# looks like some variance
qqnorm(dev_res)
qqline(dev_res)
hist(dev_res, breaks=15)
shapiro.test(dev_res)#normality test
library(car)
ncvTest(gamma_fit)# test of variance
library(ggplot2)
fitted_res <-data.frame(fitted=factor14$fitted.values, residuals=dev_res)
fitted_res <-ggplot(data=fitted_res, aes(x=fitted, y=residuals))
fitted_res + geom_point(size=3)
car::vif(factor14)

plot(factor14, which = 4, id.n = 6)
install.packages('boot')
library(boot)
glm.diag.plots(factor14, glmdiag = glm.diag(factor14))
