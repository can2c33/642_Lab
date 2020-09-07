gfr <- read.delim("GlastonburyFestivalRegression.dat", header = TRUE)
library(car)

#automatic dummy coding
autodummy <- lm(change ~ music, data = gfr, na.action = na.omit)
summary(autodummy)


#manual groups
contrasts(gfr$music) <- contr.treatment(4, base = 4)

#linear model
manual1 <- lm(change ~ music, data = gfr, na.action = na.omit)
summary(manual1)

#specifies contrasts, or variable names
crusty_v_NMA <- c(1, 0, 0, 0)
indie_v_NMA <- c(0, 1, 0, 0)
metal_v_NMA <- c(0, 0, 1, 0)

#we want these in the same variable space as the origional variables
contrasts(gfr$music) <- cbind(crusty_v_NMA, indie_v_NMA, metal_v_NMA)
manual2 <- lm(change ~ music, data = gfr, na.action = na.omit)
summary(manual2)