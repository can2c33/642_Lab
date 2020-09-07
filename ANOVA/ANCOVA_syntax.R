library(car)
library(compute.es)
library(ggplot2)
library(multcomp)
library(pastecs)

install.packages("effects")
library(effects)

#call the data and name it viagraANCOVA
viagrANCOVA <- read.delim("ViagraCovariate.dat", header = TRUE)
#view 5 lines
viagrANCOVA[1:5,]

#Train R what to call our variables
viagrANCOVA$dose <- factor(viagrANCOVA$dose, levels = c(1:3), labels = c("placebo", "low", "high"))

#Checking Assumptions for libido
by(viagrANCOVA$libido, viagrANCOVA$dose, stat.desc, basic = F, norm = T)

#assumptions for partner libido
by(viagrANCOVA$partnerLibido, viagrANCOVA$dose, stat.desc, basic = F, norm = T)

#Levenes test
leveneTest(viagrANCOVA$libido, viagrANCOVA$dose)

#levenes test partner - levenes test for the covariate
leveneTest(viagrANCOVA$partnerLibido, viagrANCOVA$dose)

#a non significant result is good, means a non-dependent effect and can run an ancova
indep <- aov(partnerLibido ~ dose, data = viagrANCOVA, na.action = na.omit)
summary(indep)

#homogeneity of regression slopes of ANCOVA not always bad
interaction <- ggplot(viagrANCOVA, aes(libido, partnerLibido, color = dose)) + geom_point() + labs(x = "libido", y = "partnerLibido", color = "dose", title = "Interaction Example") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method = "lm", se = F)
interaction

#order of enry matter, there are multiple ways to count sum of squares
expeffect <- c(-2, 1, 1)
doseeffect <- c(0, -1, 1)
contrasts(viagrANCOVA$dose) <- cbind(expeffect, doseeffect)
ancovaModel <- aov(libido ~ partnerLibido + dose, data = viagrANCOVA, na.action = na.omit)
summary(ancovaModel)

#change the way that sss is calculated
Anova(ancovaModel, type = "III")

#to look at marginal means
marginal <- effect("dose", ancovaModel, se = TRUE)
summary(marginal)

#standard errors
marginal$se

#look at planned contrasts
summary.lm(ancovaModel)

#posthoc Tukey
phtancova <- glht(ancovaModel, linfct = mcp(dose = "Tukey"))
summary(phtancova)

#Post hoc Dunnett - if groups exhibit unequal sample size
phdancova <- glht(ancovaModel, linfct = mcp(dose = "Dunnett"), base = 1)
summary(phdancova)

#confidence interval
confint(phdancova)

#The following inspects residuals
viagrANCOVA$stdresid <- rstandard(ancovaModel)

#
stat.desc(viagrANCOVA$stdresid, basic = FALSE, norm = TRUE)

#
leveneTest(viagrANCOVA$stdresid, viagrANCOVA$dose)

#unequal group sizes
indep <- aov(partnerLibido ~ dose, data = viagrANCOVA, na.action = na.omit)
summary(indep)

#inspective the regression slopes
interaction <- ggplot(viagrANCOVA, aes(libido, partnerLibido, color = dose)) + geom_point() + labs(x = "libido", y = "partnerLibido", color = "dose", title = "Interaction Example") + theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method = "lm", se = F)
interaction

#evaluating the homogeinity of the regression slope
slopes <- aov(libido ~ partnerLibido + dose + dose:partnerLibido, data = viagrANCOVA, na.action = na.omit)
summary(slopes)

#need this output, but we do not have numbers, we just know there is a difference
Anova(slopes, type = "III")


#Fitting Ancova
expeffect <- c(-2, 1, 1)
doseeffect <- c(0, -1, 1)
contrasts(viagrANCOVA$dose) <- cbind(expeffect, doseeffect)
ancovaModel <- aov(libido ~ partnerLibido + dose, data = viagrANCOVA, na.action = na.omit)
summary(ancovaModel)

#type III SSS
Anova(ancovaModel, type = "III")

#
marginal <- effect("dose", ancovaModel, se = TRUE)
summary(marginal)

#
marginal$se

#
summary.lm(ancovaModel)

#
phtancova <- glht(ancovaModel, linfct = mcp(dose = "Tukey"))
summary(phtancova)

#
phdancova <- glht(ancovaModel, linfct = mcp(dose = "Dunnett"), base = 1)
summary(phdancova)

#
confint(phdancova)

#
slopes <- aov(libido ~ partnerLibido + dose + dose:partnerLibido, data = viagrANCOVA, na.action = na.omit)
summary(slopes)

#
slopesalt <- aov(libido ~ partnerLibido*dose, data = viagrANCOVA, na.action = na.omit)
summary(slopesalt)

#
Anova(slopes, type = "III")

