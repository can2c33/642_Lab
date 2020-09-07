
#automatic dummy coding
autodummy <- lm(emotion ~ type, data = emo, na.action = na.omit)
summary(autodummy)

#manual groups
contrasts(emo$type) <- contr.treatment(3, base = 3)

#linear model
manual1 <- lm(emotion ~ type, data = emo, na.action = na.omit)
summary(manual1)

#specifies contrasts, or variable names
hippie_v_socio <- c(0, 1, 0)
hippie_v_stoic <- c(0, 0, 1)

#we want these in the same variable space as the origional variables
contrasts(emo$type) <- cbind(hippie_v_socio, hippie_v_stoic)
manual2 <- lm(emotion ~ type, data = emo, na.action = na.omit)
summary(manual2)