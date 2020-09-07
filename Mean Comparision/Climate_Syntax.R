Orgwide <- read.delim("OrgWide.dat", header = TRUE)
orgwide <- read.csv(file = 'OrgWide.csv')
head(orgwide)

library(ggplot2)
library(pastecs)
library(WRS2)
library(car)

?stat.desc
#test normality but is not groups
stat.desc(orgwide, basic = F, norm = T)

#we want to use this to use the groupi
#normality 
by(anxLong$Anxiety,anxLong$Group, stat.desc, basic = F, norm = T)

#for homogeniety significance bad
leveneTest(anxLong$Anxiety, anxLong$Group)

#use this when the data are in long columns
longModel <- t.test( ~ Group, data = anxLong, paired = FALSE, na.action = na.omit)
longModel

#
wideModel <- t.test(orgwide$Perform1, orgwide$Perform2, paired = FALSE, na.action = na.omit)
wideModel

source(file.choose())

yuen.effect(orgwide$Perform1, orgwide$Perform2, tr = .2, alpha = .05) #use this with the text file
#$dif is mean differences $crit is the value of t when probability is significant
#$var explained R2 $Effect Size is R

#can also use the wide model
talt <- wideModel$statistic[[1]]
dfalt <- wideModel$parameter[[1]]

#this creates the actual effect size and fthe following code rounds it if you want it.
r <- sqrt(t^2/(t^2+df))
round (r, 3)
