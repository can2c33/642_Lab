anxLong <- read.delim("SpiderLong.dat", header = TRUE)
anxWide <- read.delim("SpiderWide.dat", header = TRUE)
library(ggplot2)
library(pastecs)
library(WRS)
library(car)

anxWide$dif <- anxWide$picture - anxWide$real
anxWide

#you do not have to test for homogeneity of variance in a dependent t test
stat.desc(anxWide$dif, basic = F, norm = T)

#D = mean difference divided by the standard error becasue thedifference of the mean is equal to zero
longModeldep <- t.test(Anxiety ~ Group, data = anxLong, paired = TRUE, na.action = na.omit)
longModeldep  #make sure you use paired = TRUE

#
wideModeldep <- t.test(anxWide$picture, anxWide$real, paired = TRUE, na.action = na.omit)
wideModeldep

#to account for the dependent nature of it
yuend(anxWide$picture, anxWide$real, tr = .2)

#
ydbt(anxWide$picture, anxWide$real, tr = .2, nboot = 2000, alpha = .05, side = TRUE)

#
bootdpci(anxWide$picture, anxWide$real, alpha = .05, nboot = 2000, est = tmean)


t <- longModeldep$statistic[[1]]
df <- longModeldep$parameter[[1]]

talt <- wideModeldep$statistic[[1]]
dfalt <- wideModeldep$parameter[[1]]

r <- sqrt(t^2/(t^2+df))

round(r, 3)

