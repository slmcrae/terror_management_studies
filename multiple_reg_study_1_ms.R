## Minor Thesis - rehash- Terror Management Theory in a Multicultural Society


setwd("C:/Users/sherr/Desktop/Minor_Thesis/terror_management")
msd <- read.csv("study_1_MS_analysis_spread.csv")
library(dplyr)
msd <-rename(msd,PCA3=ï..PCA3)
View(msd)
msd <- msd[-c(95,93,70),]

#------------------------------------------
#  Box cox transform:

library(MASS)
y <- ms$y
x1 <- ms$self_esteem
x2 <- ms$nat_identity

bc <- boxcox(y_nonneg~x1 + x2) ### THIS WORKS WITH SAME PLOT AS BOXCOX.ar !!!
best_lambda <- bc$x[which.max(bc$y)]
round(best_lambda,1) ## i
# can't do log with neg values
min(ms$y)
y_nonneg <- ms$y+3
bc_y <- log(y_nonneg)
# best lambda is 1.6 for y_nonneg
lambda = 1.6

y_boxcox= ((y_nonneg^lambda)-1)/lambda
boxcox_fit <- lm(y_boxcox ~ x1 +x2)
summary(boxcox_fit)

shapiro.test(residuals(boxcox_fit)) # normal dist if lambda is 1.6
# only just normally distributed if lamda is 2

par(mfrow=c(2,2))
plot(boxcox_fit, which=1)
plot(boxcox_fit, which=2)
plot(boxcox_fit, which=3)
plot(boxcox_fit, which=5)
par(mfrow=c(1,1))

library(car)
ncvTest(boxcox_fit) # borderline variance. Use glm?
# variance constant: FAIL to reject null hypothesis
hist(residuals(boxcox_fit))

res_std=scale(residuals(boxcox_fit))
plot(res_std ~ boxcox_fit$fitted.values)

durbinWatsonTest(boxcox_fit)
plot(residuals(boxcox_fit), type="o")
abline(h=0, col='red')
# no autocorrelation


#  END BOX COX MANIPULATION of RESPONSE VARIABLE
#--------------------------------------------------------------------
ms_linear<-lm(wdefense~ sePC1 +sePC2 + sePC3 + factor(ms)+ idPC1 +
                sePC1:sePC2 + sePC1:factor(ms) + sePC1:idPC1 +
                sePC1:sePC3 + sePC2:factor(ms) + sePC2:idPC1 +
                sePC2:sePC3 + factor(ms):idPC1 + factor(ms):sePC3 +
                idPC1:sePC3, data=msd)

ms_linear<-lm(wdefense~ -1+ sePC1 + idPC1, data=msd)
AIC(ms_linear)
BIC(ms_linear)
summary(ms_linear)
confint.default(ms_linear)
anova(ms_linear)
library(car)
Anova(ms_linear, type="III")
?Anova

# best model:
library(MASS)
stepReg=MASS::stepAIC(ms_linear, direction="both")
stepReg$anova

library(car)
Anova(ms_linear) #(Anova-not anova-order of variable doesn't matter)# same as summary
anova(ms_linear)
plot(ms_linear)

par(mfrow=c(2,2))
plot(ms_linear, which=1)
plot(ms_linear, which=2)
plot(ms_linear, which=3)
plot(ms_linear, which=5)
par(mfrow=c(1,1))

res_std=scale(residuals(ms_linear))
res_std=scale(msd$jew)
plot(res_std ~ ms_linear$fitted.values)
#
shapiro.test(residuals(ms_linear))
# not normally distributed with all interaction terms: null hypothesis rejected
library(car)
durbinWatsonTest(ms_linear)
plot(residuals(ms_linear), type="o")
abline(h=0, col='red')
# errors uncorrelated: FAIL to reject null hypothesis
ncvTest(ms_linear)
# variance constant: FAIL to reject null hypothesis
hist(residuals(ms_linear), main="Histogram of residuals: Response variable wdefence")

library(ggplot2)
run_color_coded<-data.frame(fitted=ms_linear$fitted.values, residuals=ms_linear$residuals, ms=ms$ms_factor )
res_plot<-ggplot(data=run_color_coded, aes(x=fitted, y=residuals, colour=factor(ms)))
res_plot + geom_point(size=3)
# this shows that two different runs have different slopes

# multcolinearity?
car::vif(ms_linear)
# no multicollinearity as all under 1.002
plot(msd[,c(1,2,3,4)])
cor(msd[,c(5,6,7,8)])
# does not appear to be multicollinearity
# ------------------------------------------------------
#  ----------------------------------------------------

# Loading calculations:
library(Gifi)
??Gifi

dutch_emo <- ms[,7:18]
mus_emo <-ms[,19:30]
anti_dut_ess <- ms[,32:39]
mus_att <- msd[,41:44]
minor_att <- ms[,46:57]
se10 <- msd[,58:67]
id3 <- ms[,64:66]
all_response <-ms[,c(5,85:86)]
mlatt_PC <- msd[,86:87]
PCA3 <- msd[,100:104]
View(se10)
dim(msd)
??princals
citation(package="Gifi")
## ordinal PCA
fitord <- princals(se10, ndim=1)  ## ordinal PCA
fitord
summary(fitord)
plot(fitord, plot.type = "transplot")
plot(fitord, "loadplot", main = "Loadings Plot se10 Data")  ## aspect ratio = 1
plot(fitord, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord, "screeplot")

fitord_p <- princals(mus_emo, ndim=4)  ## ordinal PCA
fitord_p
summary(fitord_p)
plot(fitord_p, plot.type = "transplot")
plot(fitord_p, "loadplot", main = "Loadings Plot piet8 Data")  ## aspect ratio = 1
plot(fitord_p, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_p, "screeplot")

fitord_i <- princals(anti_dut_ess, ndim=3)  ## ordinal PCA
fitord_i
summary(fitord_i)
plot(fitord_i, plot.type = "transplot")
plot(fitord_i, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_i, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_i, "screeplot")

fitord_piets <- princals(mus_att, ndim=2)  ## ordinal PCA
fitord_piets
summary(fitord_piets)
plot(fitord_piets, plot.type = "transplot")
plot(fitord_piets, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_piets, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_piets, "screeplot")

fitord_m <- princals(minor_att, ndim=4)  ## ordinal PCA
fitord_m
summary(fitord_m)
plot(fitord_m, plot.type = "transplot")
plot(fitord_m, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_m, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_m, "screeplot")

fitord_s <- princals(se10, ndim=3)  ## ordinal PCA
fitord_s
summary(fitord_s)
plot(fitord_s, plot.type = "transplot")
plot(fitord_s, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_s, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_s, "screeplot")

fitord_d <- princals(id3, ndim=1)  ## ordinal PCA
fitord_d
summary(fitord_d)
plot(fitord_d, plot.type = "transplot")
plot(fitord_d, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_d, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_d, "screeplot")

fitord_all <- princals(all_response, ndim=5)  ## ordinal PCA
fitord_all
summary(fitord_all)
plot(fitord_all, plot.type = "transplot")
plot(fitord_all, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_all, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_all, "screeplot")

fitord_mPC <- princals(mlatt_PC, ndim=1)  ## ordinal PCA
fitord_mPC
summary(fitord_mPC)
plot(fitord_mPC, plot.type = "transplot")
plot(fitord_mPC, "loadplot", main = "Loadings Plot id3 Data")  ## aspect ratio = 1
plot(fitord_mPC, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord_mPC, "screeplot")


################################################
#  ---------------------------------------------

# Using all individual questions as factors.


x1 <- msd$se1_ind
x2 <- msd$se2_ind
x3 <- msd$se3_ind
x4 <- msd$se4_ind
x5 <- msd$se5_ind
x6 <- msd$se6_ind
x7 <- msd$se7_ind
x8 <- msd$se8_ind
x9 <- msd$se9_ind
x10 <- msd$se10_ind
x11 <- msd$id1_ind
x12 <- msd$id2_ind
x13 <- msd$id3_ind
x14 <- msd$coded_ms
x15 <- msd$pair_hse_ms
x16 <-msd$int_111

int_x3x12 <- msd$se3_ind*msd$id2_ind*msd$coded_ms
View(int_x3x12)
median(msd$se3)
ms <- msd$ms
se <- msd$self_esteem
id <- msd$nat_identity
ms <- msd$coded_ms
seXid <- msd$ZsemeanXident
seXms <- msd$ZsemeanXms
seXidXms <-msd$ZmsXsemeanXident
idXms <- msd$ZmsXident
int1 <- msd$int_111
int2 <- msd$int_001
int3<- msd$se3*msd$id3*msd$coded_ms
# mca2, mca12, anti1 - x2,x10
#
y <- msd$PCA3
min(y)
hist(y, main="Histogram of Question mca12", )
plot(density(y))
u <- mean(y)
std <-sd(y)
min(std_y)
std_y <- y*-1
std_y <- (y-u)/std
trans_y <- sqrt(5+std_y)
trans_y <- 1/(max(y)+1-y)
trans_y <- sqrt(max(std_y)+1-std_y)
min(trans_y)
plot(density(trans_y))
shapiro.test(trans_y)
plot(trans_y~msd$wdefense, main="Correlation between wdefense and transformed mlattPCA",
     xlab="Response Variable: wdefense", ylab="Response Variable: transformed mlattPCA")
cor(trans_y,msd$wdefense)
plot(density(y), main="Density Plot for Response Variable: wmus", 
     xlab="Response Variable: wmus")
library(car)
shapiro.test(std_y)
se <- msd[,60:69]
plot(y)
plot(msd[,c(2,64:69)])# all the self esteem questions have been adjusted
plot(y, data=msd, type="l")
levels(x5)
max(std_y)
x15 <- msd$indInt_3
# best so far: se6 and id3 int term
# also best pairwise term: x14:se3
factor14 <- lm(trans_y~se+id+factor(ms):se:id,data=msd)
# trans_y~factor(x1)+factor(x2)+factor(x3)+
#   factor(x4)+factor(x5)+factor(x6)+factor(x7)+
#   factor(x8)+factor(x9)+factor(x10)+factor(x11)+
#   factor(x12)+factor(x13)+factor(x14)
complex_model <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+factor(x14)+factor(x14):x3:x13,data=msd)

complex_model <- glm(trans_y~factor(x1)+factor(x2)+factor(x3)+
                      factor(x4)+factor(x5)+factor(x6)+factor(x7)+
                      factor(x8)+factor(x9)+factor(x10)+factor(x11)+
                      factor(x12)+factor(x13)+factor(x14)+factor(x15)+factor(x16),family=Gamma(link="identity"),data=msd)
??glm()
summary(factor14)
confint(factor14)
summary(complex_model)
anova(factor14)
AIC(factor14)
AICmanual <-nrow(msd)*(log(2*pi)+1+log((sum(factor14$residuals^2)/nrow(msd))))+((length(factor14$coefficients)+1)*2)
AICmanual
shapiro.test(residuals(factor14))
??factor()
library(car)
shapiro.test(residuals(factor14))
ncvTest(factor14)
durbinWatsonTest(factor14)
factor14$residuals
msXse <- se*ms
idXms <- id*ms
seXid <- se*ms
msXseXid <- ms*se*id
cor_matrix <- cbind.data.frame(ms,se,id,msXseXid, msXse, idXms,seXid)
class(cor_matrix)
plot(d2, d4)
car::vif(factor14)
??vif
attributes(alias(factor14)$Complete)$dimnames[[1]]

#   ________________________________________________________
#   ________________________________________________________
#   GLM() residuals checks:
pchisq(factor14$deviance, df=factor14$df.residual, lower.tail=FALSE)
dev_res <- residuals(factor14, type='deviance') # type= pearson or deviance
plot(dev_res, type='o')# autocorrelation
abline(h=0, col="red")
??abline
plot(dev_res ~ factor14$fitted.values)# possible outlier at 7
qqnorm(dev_res)
qqline(dev_res)
plot(density(dev_res),main="Density Plot Deviance Residuals")
library(boot)
glm.diag.plots(factor14, glmdiag = glm.diag(factor14))
shapiro.test(dev_res)
cor(msd[,c(2,3,4,5,6)])


anova(factor14, complex_model, test="Chi")# test of sig 
par(mfrow=c(2,2))
plot(factor14, which=1)
plot(factor14, which=2)
plot(factor14, which=3)
plot(factor14, which=5)
par(mfrow=c(1,1))

#   ____________________________________________________
#   ----------------------------------------------------

# after model selection: results show 3 questions are significant
sePC1 <- msd$sePC1
sePC2 <- msd$sePC2
sePC3 <- msd$sePC3
idPC1 <- msd$idPC1
ms <- msd$coded_ms
hist(trans_y, breaks=15, main= "Histogram of Transformed Response Variable", xlab="trans_y")

factor14 <- lm(PCA3~factor(x3),data=msd)
factor14 <- lm(PCA3~sePC1 + idPC1,data=msd)

summary(factor14)
# testing questions alone:
# x3(positive)  x4(positive) x6(positive) x7(v.pos) x8(v.pos) x11(pos) with wdefense
# x2 (neg,borderline) x3(neg) x4(neg) x5(neg,borderline) x6(neg) x7(neg, borderline) x8(neg,borderline) x11(neg, borderline)
#   PCA3 ~ factor(x4) + factor(x13)
# reason some of these are not significant with others is because
# they haven't been reverse scored?
library(MASS)
stepReg=MASS::stepAIC(factor14, direction="both")
stepReg$anova
# for muslims: x4, x12,x13,x14
# wdefense: x4, x9, x11, x12, x13, x14
# PCA3 PCA3 ~ x4 + x10 + x11 + x12 + x14
all_poss_model <-leaps::regsubsets(y~factor(x1)+factor(x2)+factor(x3)+factor(x4)+factor(x5)+
                                     factor(x6)+factor(x7)+factor(x8)+factor(x9)+factor(x10)+
                                     factor(x11)+factor(x12)+factor(x13)+factor(x14)+factor(x15),
                                   data=msd)
summary(all_poss_model)
plot(all_poss_model, scale="bic", main="Model Selection: mca12 variables assumed metric") # scale = r, r2, adjr2,Cp
??regsubsets
# scale =c("bic", "Cp","adjr2","r2")
library(car)
shapiro.test(residuals(factor14))
ncvTest(factor14)
durbinWatsonTest(factor14)
plot(density(residuals(factor14)), main="Density Plot of Residuals with Normal Curve Overlay")

ur <- mean(residuals(factor14))
stdr <- sd(residuals(factor14))
stdr
std_r <- (residuals(factor14)-ur)/stdr

plot(density(std_r), main="Standardized Residuals with Normal Curve Overlay",
     xlab="Formula: y ~ x2 + x13")
curve(dnorm(x, mean=0,sd=1.9),col=10, add=TRUE)
car::vif(factor14)
stdr
z<- dnorm(x, mean=0,sd=2)
plot(density(z))
par(mfrow=c(2,2))
plot(factor14, which=1)
plot(factor14, which=2)
plot(factor14, which=3)
plot(factor14, which=5)
par(mfrow=c(1,1))

plot(residuals(factor14), type="o", main="Residuals", ylab="Residuals from Model: y ~ se2 + indent3")
abline(h=0, col='red')
cor(msd[,c(59,60,61,64,65,66,69,70)])# for self esteem questiosn
cor(msd[,c(68:70)])# for identity questions
cor(msd[,c(49:60)])# for acceptance of ethnic minorities
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(factor14)
summary(gvmodel)
??gvlma
hist(y,main="Histogram of Question mca12 Responses", xlab="Higher scores indicate increased acceptance of ethnic minorities")
cor(x3,int_111)

library(MASS)
x2.freq=table(x2)
x3.freq=table(x3)
x12.freq=table(x12)
x13.freq=table(x13)
par(mfrow=c(2,2))
barplot(x2.freq, main="Histogram of Self-Esteem Item 2", ylab="Frequency",
        xlab="Response (Scale: 1-9)")
barplot(x3.freq, main="Histogram of Self-Esteem Item 3", ylab="Frequency",
        xlab="Response (Scale: 1-9)")
barplot(x12.freq, main="Histogram of National Identity Item 2", ylab="Frequency",
        xlab="Response (Scale: 1-9)")
barplot(x13.freq, main="Histogram of National Identity Item 3", ylab="Frequency",
        xlab="Response (Scale: 1-9)")
par(mfrow=c(1,1))
y <-msd$mca12
y.freq=table(y)
barplot(y.freq, main="Histogram of Ordinal Response Variable: mca12", ylab="Frequency",
        xlab="Response (Scale: 1-9)")
median(x3)
#x2 split at 8,9/ other
par(mfrow=c(1,2))
plot(density(msd$ï..y), main="Original Response Variable Density Plot",
     xlab="Standardized Response Variable (49 items included)")
plot(density(trans_y),main="PCA Response Variable Density Plot",
     xlab="Transformed PCA Response Variable") 
par(mfrow=c(1,1))

int_111 <- factor(msd$int_111)
library(MASS)
polr(factor(y)~factor(x13)+factor(x15), data=msd, method="probit")
