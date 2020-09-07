viagra <- read.delim("Viagra.dat", header = TRUE)
library(car)
library(pastecs)

#dose of viagra is the IV and dose is DV, regression and ANOVS rely on the general LM
#name of data set$name of variable <- factor the levels 1 to 3, in order from 1, 2, 3
viagra$dose <- factor(viagra$dose, levels = c(1:3), labels = c("placebo","low","high"))

#at this point we are not creating reference groups, we are letting R do it, and R
#always chooses the first group in alphabetical order
dummy <- lm(libido ~ dose, data = viagra, na.action = na.omit)
summary(dummy)

#in order to understand the low dose you have to add it to the placebo group
#the high group add the high dose estimate and the intercept estimate
#what the F test tells you in regression is the same as the f statistic in ANOVA

library("compute.es")
library("multcomp")

#aov is the function for a non robust ANOVA, when considering assumption, they are the same
#normality by group, interval, independent, homogeineity of variance
#as long as your groups are of equal size, when unequal, then normality
#has significant impact

viagraModel <- aov(libido ~ dose, data = viagra, na.action = na.omit)
summary(viagraModel)

#use this to begin to inspect assumption
by(viagra$libido, viagra$dose, stat.desc, basic = F, norm = T)
leveneTest(viagra$libido, viagra$dose)
summary(viagraModel)

#after assumption, and before planned contracts
viagraModel <- aov(libido ~ dose, data = viagra, na.action = na.omit)
summary(viagraModel)
#if assumptions not met
#welches F test, the df has changed because of the Welches test and our P test has increased
oneway.test(libido ~ dose, data = viagra)

#planned contrasts
contrast1 <- c(-2,1,1)
contrast2 <- c(0,-1,1)
contrasts(viagra$dose) <- cbind(contrast1, contrast2)
viagraPlanned <- aov(libido ~ dose, data = viagra)
summary.lm(viagraPlanned)

#to see trends
contrasts(viagra$dose) <- contr.poly(3)
viagraTrend <- aov(libido ~ dose, data = viagra)
summary.lm(viagraTrend)


#adjusted bonferroni
pairwise.t.test(viagra$libido, viagra$dose, paired = FALSE, p.adjust.method = "bonferroni")

#Tukey's, equal sample size and homogeinity of variance
library(multcomp)
postHocT <- glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(postHocT)

#confidence interval Tukeys
confint(postHocT)

install.packages("userfriendlyscience")
library(userfriendlyscience)

viagraModelalt <- oneway(viagra$libido, viagra$dose, levene = TRUE, posthoc = "games-howell", digits = 3, corrections = TRUE)
viagraModelalt
