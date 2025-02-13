library(survival)

ID <- c(1,2,3,4,5,6)
TIME <- c(8,9,10,11,11,12)
EVENT <- c(1,0,1,1,1,0)
DATA <-  data.frame(ID,TIME,EVENT)
SURVIVAL01 <- survfit(Surv(DATA$TIME,DATA$EVENT) ~ 1)
plot(SURVIVAL01)
summary(SURVIVAL01)