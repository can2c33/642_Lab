
install.packages("WRS2")

source(file.choose())

anxLong <- read.delim("SpiderLong.dat", header = TRUE)
anxWide <- read.delim("SpiderWide.dat", header = TRUE)
library(ggplot2)
library(pastecs)
library(WRS2)
library(car)

#test normality but is not groups
stat.desc(anxLong$Anxiety, basic = F, norm = T)

#we want to use this to use the groupi
#normality 
by(anxLong$Anxiety,anxLong$Group, stat.desc, basic = F, norm = T)

#for homogeniety significance bad
leveneTest(anxLong$Anxiety, anxLong$Group)

#use this when the data are in long columns
longModel <- t.test(Anxiety ~ Group, data = anxLong, paired = FALSE, na.action = na.omit)
longModel

#
wideModel <- t.test(anxWide$picture, anxWide$real, paired = FALSE, na.action = na.omit)
wideModel

#This is a comparision usinf the general linear model instead of a t-test
proof <- lm(Anxiety ~ Group, anxLong, na.action = na.omit)
summary(proof)

#Robust examination
#trimmed means trimms the extreme data on each side of the mean.
yuen(Anxiety ~ Group, anxLong, tr = .2) #use this with WS2 package

yuen.effect(anxWide$picture, anxWide$real, tr = .2, alpha = .05) #use this with the text file
#$dif is mean differences $crit is the value of t when probability is significant
#$var explained R2 $Effect Size is R

#combining the trimmed mean with bootstrapped for kertosis and normality
yuenbt(Anxiety ~ Group, anxLong, tr = .2, nboot = 2000, side = TRUE)
#side TRUE means that the sides will be equal

#tells R wants to use the estimator mom, a modified imputation
pb2gen(Anxiety ~ Group, anxLong, est = "mom", nboot = 2000)

#Effect Sizes
t <- longModel$statistic[[1]]
df <- longModel$parameter[[1]]

#can also use the wide model
talt <- wideModel$statistic[[1]]
dfalt <- wideModel$parameter[[1]]

#this creates the actual effect size and fthe following code rounds it if you want it.
r <- sqrt(t^2/(t^2+df))
 round (r, 3)
 