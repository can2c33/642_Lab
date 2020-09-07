hw2data <- read.delim("HW2.dat", header = TRUE)
library(ggplot2)
library(pastecs)
library(psych)
library(boot)
library(QuantPsyc)
library(car)
#1-3 based on class notes/book knowledge
#4-10
stat.desc(hw2data[,c("Stigma", "Thoughts", "Hope", "Emotion")], basic = F, norm = T)
#11-14 based on class notes/book knowedge
#15-19
model1 <- lm(Stigma ~ Thoughts, data = hw2data, na.action = na.omit)
summary(model1)
#20
lm.beta(model1)
#21 based on class notes/book knowledge
#22
#-0.28 * 1.5= -0.42 (answer to #20 times 1.5 standard deviations = this number)
#-0.42 * 7.55 = -3.17 (number from above times the standard deviation for stigma =)
# -3.17 + 39.21 = 36.04 (the above number times #19)
stat.desc(hw2data[,c("Stigma", "Thoughts")], basic = F, norm = T) #to find SD of Stigmafor 22
#23-27
model2dat <- read.delim("HW2.dat", header = TRUE)
model2 <- lm(Stigma ~ Thoughts + Hope + Emotion, data = hw2data, na.action = na.omit)
summary(model2)
#23 for 23, r^2 is 0.01031 * 100 = 10.31%
#28-30
lm.beta(model2)
#31 (r^(2) model 2) - (r^(2) model 1) *100  = 0.1031 - 0.07678 = -0.02632 *100 = 2.63
#32-34
anova(model1, model2)
#35-36 9/2=.045*100= 4.5%
rstandard(model2)
model2dat$stdresid <- rstandard(model2)
model2dat$large.resid <- model2dat$stdresid > 2 | model2dat$stdresid < -2
sum(model2dat$large.resid)
#37
problems <- model2dat[model2dat$large.resid,] #if before the column you want the row, 
problems
model2dat$cooks <- cooks.distance(model2)
model2dat$leverage <- hatvalues(model2)
model2dat$dffit <- dffits(model2)
model2dat$dfbeta <- dfbeta(model2)
problems <- model2dat[model2dat$large.resid,c("cooks", "leverage", "dffit", "dfbeta")]
problems
#38 (k+1)/n 3+1/200 = 4/200= .02
#39 looked at them
#40
ncvTest(model2)
#41 p value
#42
modelpredict <- model2dat[,c("Thoughts","Hope","Emotion")]
cor(modelpredict, use = "pairwise.complete.obs", method = "pearson")
#43-44
vif(model2)
#added the three together and divided by 3
#45-47
durbinWatsonTest(model2)
#48
boot.ci(boot.out = boot_r, type = "bca", index = 1)
#49  notes