hw2data <- read.delim("HW2.dat", header = TRUE)
library(ggplot2)
library(pastecs)
library(psych)
library(boot)
library(QuantPsyc)
library(car)

#4-10
stat.desc(hw2data[,c("Stigma", "Thoughts", "Hope", "Emotion")], basic = F, norm = T)

#15-19
model1 <- lm(Stigma ~ Thoughts, data = hw2data, na.action = na.omit)
summary(model1)

#20-21
lm.beta(model1)

#22
stat.desc(hw2data[,c("Stigma", "Thoughts")], basic = F, norm = T) #to find SD of Stigmafor 22

#23-27
model2dat <- read.delim("HW2.dat", header = TRUE)
model2 <- lm(Stigma ~ Thoughts + Hope + Emotion, data = hw2data, na.action = na.omit)
summary(model2)

#28-30
lm.beta(model2)

#32-34
anova(model1, model2)

#35-36
rstandard(model2)
model2dat$stdresid <- rstandard(model2)
model2dat$large.resid <- model2dat$stdresid > 2 | model2dat$stdresid < -2
sum(model2dat$large.resid)

#37
problems <- model2dat[model2dat$large.resid,] 
problems

model2dat$cooks <- cooks.distance(model2)
model2dat$leverage <- hatvalues(model2)
model2dat$dffit <- dffits(model2)
model2dat$dfbeta <- dfbeta(model2)
problems <- model2dat[model2dat$large.resid,c("cooks", "leverage", "dffit", "dfbeta")]
problems

#40-41
ncvTest(model2)

#42
modelpredict <- model2dat[,c("Thoughts","Hope","Emotion")]
cor(modelpredict, use = "pairwise.complete.obs", method = "pearson")

#43-44
vif(model2)

#45-47
durbinWatsonTest(model2)

#48
boot.ci(boot.out = boot_r, type = "bca", index = 1)
