

read.delim("Festival.dat", header = TRUE) #this tells R to read the file, but it does not 
#bring it in to the R data environment


fdata = read.delim("Festival.dat", header = TRUE) #this assigns all the data to the name 
#fdata and tells R that there are variable names in the data and not to use them

fdata #all this does is prints your data in the console

library(ggplot2) #this activates ggplot and the following activates the rest of the packages
library(car)
library(pastecs)
library(psych)
library(carData)



