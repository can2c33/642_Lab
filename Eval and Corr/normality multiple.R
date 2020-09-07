eData = read.delim("RExam.dat", header = TRUE)
eData <- factor(eData$uni, levels = c(0:1), labels = c("Duncetown University", "Sussex University"))
eData
eData <- as.data.frame(eData)
hist.computer = ggplot(eData, aes(computer)) + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + labs(x = "computer", y = "Density")

hist.lectures = ggplot(eData, aes(lectures)) + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + labs(x = "lectures", y = "Density")
hist.numeracy = ggplot(eData, aes(numeracy)) + geom_histogram(aes(y = ..density..), color = "black", fill = "white") + labs(x = "numeracy", y = "Density")
hist.computer


dunceData <- subset(eData, eData$uni=="Duncetown University")
sussexData <- subset(eData, eData$uni=="Sussex University")

