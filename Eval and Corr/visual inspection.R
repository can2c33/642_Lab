library(ggplot2) #this activates ggplot and the following activates the rest of the packages
library(car)
library(pastecs)
library(psych)
library(carData)

hist.day3 = ggplot(fdata, aes(day3)) + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + labs(x = "Hygiene score on day 3", y = "Density")

hist.day3

hist.day3 + stat_function(fun = dnorm, args = list(mean(fdata$day3, na.rm = TRUE), sd = sd(fdata$day3, na.rm = TRUE)), Colour = "black", size = 1)

qqplot.day3 = qplot(sample = fdata$day3, stat= "qq") #what this function does is compare your data to 

qqplot <- ggplot(fdata, aes(sample(day1)) + stat_qq(na.rm = TRUE))
qqplot

qqplot <- ggplot(fdata, aes(sample = day3)) + stat_qq(na.rm = TRUE)
qqplot

describe(fdata$day1)

stat.desc(fdata$day1, basic = FALSE, norm = TRUE)

describe(cbind(fdata$day1, fdata$day2, fdata$day3)) #use when you need to know the number of cases in 
  #each variable #use if you are factor analyzing a scale

stat.desc(cbind(fdata$day1, fdata$day2, fdata$day3), basic = FALSE, norm = TRUE) #use when you need to know the 
  #significance of the skew or kurtosis
  #the smaller the coef of var the leass variability in the variable


