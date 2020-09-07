library(car)
library(QuantPsyc)
library(boot)

simple <- read.delim("Album Sales 1.dat", header = TRUE)
hierarchical = read.delim("Album Sales 2.dat", header = TRUE)
gfr <- read.delim("GlastonburyFestivalRegression.dat", header = TRUE)

simplereg <- lm(sales ~ adverts, data = simple, na.action = na.omit)  #newModel<-lm(outcome ~ predictor(s), data = dataFrame, na.action = an action))

summary(simple) #must use summary simple because it is the 

options(scipen = 999) #this turns off scientfific notation
options(scipen = .999) #this turns scientific notation back on

lm.beta(simple) #give you the standardized beta coefiencent

library(psych) #access to decribe
library(pastecs) #stat.desc

describe(simple[,c("sales", "adverts")]) #this concatonates the variables to decribe
stat.desc(simple[,c("sales", "adverts")], basic = FALSE, norm = TRUE)

round(stat.desc(simple[,c("sales", "adverts")], basic = FALSE, norm = TRUE), digits = 2) #this rounds the above code

albumsales.2 <- lm(sales ~ adverts + airplay + attract, data = hierarchical, na.action = na.omit)
summary(albumsales.2)

lm.beta(albumsales.2)

anova(simplereg, albumsales.2)
anova(albumsales.2, simplereg)
