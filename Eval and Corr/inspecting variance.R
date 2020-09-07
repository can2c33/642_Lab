eData = read.delim("RExam.dat", header = TRUE)
eData$uni <- factor(eData$uni, levels = c(0:1), labels = c("Duncetown University", "Sussex University"))
eData$uni <- factor(eData$uni, levels = c(0:1), labels = c("Duncetown University", "Sussex University"))
eData

library(car)

leveneTest(eData$exam, eData$uni, center = mean)
## Warning in leveneTest.default(eData$exam, eData$uni, center = mean): eData$uni
## coerced to factor. If you want it to center around the median, you have to tell it.
## Levene's Test for Homogeneity of Variance (center = mean)
##       Df F value Pr(>F)
## group  1  2.5841 0.1112
##       98
leveneTest(eData$exam, eData$uni, center = median)
## Warning in leveneTest.default(eData$exam, eData$uni, center = median): eData$uni
## coerced to factor.
## Levene's Test for Homogeneity of Variance (center = median)
##       Df F value Pr(>F)
## group  1  2.0886 0.1516
##       98