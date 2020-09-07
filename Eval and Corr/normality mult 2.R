eData = read.delim("RExam.dat", header = TRUE)
eData$uni <- factor(eData$uni, levels = c(0:1), labels = c("Duncetown University", "Sussex University"))
eData$uni <- factor(eData$uni, levels = c(0:1), labels = c("Duncetown University", "Sussex University"))
eData

hist.exam <- ggplot(eData, aes(exam)) + geom_histogram(aes(y =  
            ..density..), color = "black", fill = "white") + labs(x = "Exam  
            Score", y = "Density")
hist.exam

hist.computer <- ggplot(eData, aes(computer)) + geom_histogram(aes(y =  
          ..density..), color = "black", fill = "white") + labs(x = "Computer  
          Literacy", y = "Density")
hist.computer

hist.lectures <- ggplot(eData, aes(lectures)) + geom_histogram(aes(y =  
          ..density..), color = "black", fill = "white") + labs(x = "Percent  
          Attendance", y = "Density")
hist.lectures

hist.numeracy <- ggplot(eData, aes(numeracy)) + geom_histogram(aes(y =  
          ..density..), color = "black", fill = "white") + labs(x = "Numeracy",  
          y = "Density")
hist.numeracy



describe(cbind(eData$exam, eData$computer, eData$lectures, eData$numeracy)) #use when you need to know the number of cases in 
#each variable #use if you are factor analyzing a scale

stat.desc(cbind(eData$day1, eData$exam, eData$lectures, eData$numeracy), basic = FALSE, norm = TRUE) #use when you need to know the 
#significance of the skew or kurtosis
#the smaller the coef of var the leass variability in the variable

describe(eData[,c("exam","computer","lectures", "numeracy")])

stat.desc(eData[,c("exam","computer","lectures", "numeracy")], basic = FALSE, norm = TRUE)

hist.exam + stat_function(fun = dnorm, args = list(mean =  
          mean(eData$exam, na.rm = TRUE), sd = sd(eData$exam, na.rm = TRUE)),  
          color = "black", size = 1)
hist.computer + stat_function(fun = dnorm, args = list(mean =  
                                                         mean(eData$computer, na.rm = TRUE), sd = sd(eData$computer, na.rm =  
                                                                                                       TRUE)), color = "black", size = 1)

hist.lectures + stat_function(fun = dnorm, args = list(mean =  
                                                         mean(eData$lectures, na.rm = TRUE), sd = sd(eData$lectures, na.rm =  
                                                                                                       TRUE)), color = "black", size = 1)

hist.numeracy + stat_function(fun = dnorm, args = list(mean =  
                                                         mean(eData$numeracy, na.rm = TRUE), sd = sd(eData$numeracy, na.rm =  
                                                                                                       TRUE)), color = "black", size = 1)

dunceData <- subset(eData, eData$uni=="Duncetown University")
dunceData
sussexData <- subset(eData, eData$uni=="Sussex University")
sussexData

round(describe(eData[,c("exam","computer","lectures", "numeracy")]), digits = 2)

round(stat.desc(eData[,c("exam", "computer", "lectures", "numeracy")], basic = FALSE, norm = TRUE), digits = 2)



dunceData <- subset(eData, eData$uni=="Duncetown University")
dunceData
sussexData <- subset(eData, eData$uni=="Sussex University")
sussexData


by(eData$exam, eData$uni, describe)
by(eData$exam, eData$uni, stat.desc, basic = FALSE, norm = TRUE)
by(eData[,c("exam", "numeracy")], eData$uni, stat.desc, basic = FALSE, norm = TRUE)
stat.desc(eData$exam, basic = FALSE, norm = TRUE)
by(eData$exam, eData$uni, stat.desc, basic = FALSE, norm = TRUE)
shapiro.test(eData$exam)
by(eData$exam, eData$uni, shapiro.test)
