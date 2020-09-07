simple <- read.delim("Album Sales 1.dat", header = TRUE)
hierarchical <- read.delim("Album Sales 2.dat", header = TRUE)

library(car)
library(QuantPsyc)
library(boot)
library(psych)
library(pastecs)
library(ggplot2)

albumsales.1 <- lm(sales ~ adverts, data = simple, na.action = na.omit)
albumsales.2 <- lm(sales ~ adverts + airplay + attract, data = hierarchical, na.action = na.omit)

rstandard(albumsales.2) #this generates a horrible long table, so do not use this alone

hierarchical$stdresid <- rstandard(albumsales.2)

hierarchical[1:5,] #this shows the first 5 rows, you need the last comma

hierarchical$sturesid <- rstudent(albumsales.2) #these two lines create the variables needed for residuals
hierarchical$resid <- resid(albumsales.2)

hierarchical$large.resid <- hierarchical$stdresid > 2 | hierarchical$stdresid < -2 #this creates the variable large.resid 

sum(hierarchical$large.resid) #returns the number of cases that are true

problems <- hierarchical[hierarchical$large.resid,] #the brackets in our data frame it is row, colum
problems

write.table(problems, "Album Sales Problems.dat", sep = "\t", row.names = TRUE)

hierarchical$cooks <- cooks.distance(albumsales.2)
hierarchical$leverage <- hatvalues(albumsales.2)
hierarchical$dffit <- dffits(albumsales.2)
hierarchical$dfbeta <- dfbeta(albumsales.2) 
problems <- hierarchical[hierarchical$large.resid,c("cooks", "leverage", "dffit", "dfbeta")] #updates your special cases dataframe

problems

df.betas <- dfbeta(albumsales.2) #downside: this gives you the figures for everyone
df.betas[1:10,] #only looking at the first 10 rather than all 200

ncvTest(albumsales.2)

library(ggplot2)

hierarchical$fitted <- albumsales.2$fitted.values
scatter <- ggplot(hierarchical, aes(fitted, sturesid)) + geom_point() + geom_smooth(method = "lm", color = "Blue", se = F) + labs(x = "Fitted Values", y = "Studentized Residual")
scatter

hpredict <- hierarchical[,c("adverts","airplay","attract")]
cor(hpredict, use = "pairwise.complete.obs", method = "pearson")

vif(albumsales.2)

1/vif(albumsales.2) #this is the reciprocal of the above line

external <- hierarchical[,c("adverts","airplay","attract","resid")] #this checks to see is any external variables are affecting out model
cor(external, use = "pairwise.complete.obs", method = "pearson")

durbinWatsonTest(albumsales.2)

dwt(albumsales.2)

resid.hist <- ggplot(hierarchical, aes(stdresid)) + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + labs(x = "Standardized Residual", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(hierarchical$stdresid, na.rm = TRUE), sd = sd(hierarchical$stdresid, na.rm = TRUE)), color = "black", size = 1)

resid.hist

resid.qq <- ggplot(hierarchical, aes(sample = stdresid)) + stat_qq(na.rm = TRUE) + labs(x = "Theoretical Values", y = "Observed Values")

resid.qq

stat.desc(hierarchical$stdresid, basic = F, norm = T)

bootReg <- function (formula, data, i)  #this is the bootstrapping code to create the bootstraped method
{
  d<- data [i,]
  fit <- lm(formula, data = d)
  return (coef(fit))
}

bootResults <- boot(statistic = bootReg, formula = sales ~ adverts + airplay + attract, data = hierarchical, R = 2000)
bootResults

interceptbtci <- boot.ci(bootResults, type = "bca", index = 1) #cf for your intercept
interceptbtci

advertsbtci <- boot.ci(bootResults, type = "bca", index = 2) #cf fpr first predictor
advertsbtci

airplaybtci <- boot.ci(bootResults, type = "bca", index = 3) #cf for 2nd predictor
airplaybtci

attractbtci <- boot.ci(bootResults, type = "bca", index = 4) #cf for 3rd predictor
attractbtci


