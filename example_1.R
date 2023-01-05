rm(list=ls())
library(ggplot2); library(psych)
theme_set(theme_bw())
# === === === === === === === === === === === === === === === === === === 

library(MPDiR)

data(HSP)

names(HSP) <- c("Quanta", "PerCent", "N", "Obs", "Run")
HSP$PerCent <- HSP$PerCent/100

str(HSP)
head(HSP)
with(HSP, table(Run, Obs))
summary(HSP)

qplot(Quanta, PerCent,
      colour=Obs, size=I(5),
      data=HSP)
qplot(log(Quanta), PerCent,
      colour=Obs, size=I(5),
      data=HSP)

qplot(Quanta, PerCent,
      colour=Obs, size=I(5),
      data=HSP) + facet_grid(Run ~ Obs)

HSP <- within(HSP, { NumYes <- round(N * PerCent)
                     NumNo  <- N - NumYes })
head(HSP)

SHR1 <- subset(HSP, Obs=="SH" & Run=="R1")
SHR1

qplot(Quanta, NumYes,
      size=I(5),
      data=SHR1) + geom_point(aes(y=NumNo), shape=I(2), size=I(5), colour=I("red"))
qplot(log(Quanta), NumYes,
      size=I(5),
      data=SHR1) + geom_point(aes(y=NumNo), shape=I(2), size=I(5), colour=I("red"))

SHR1.glm <- glm(formula = cbind(NumYes, NumNo) ~ log(Quanta),
                family = binomial(probit), data = SHR1)
summary(SHR1.glm)
anova(SHR1.glm, test = "Chisq")
coef(SHR1.glm)
confint(SHR1.glm)

# ===   predicted values
pp <- qplot(Quanta, PerCent,
      size=I(5), 
      data=SHR1) + xlab("Quanta/Flash") + ylab("Proportion \"seen\"") + 
  ggtitle("Obs: SH, Run: 1") +
  scale_x_log10(breaks=c(25,50,100,200,400), limit=c(20,440))
pp
xseq <- seq(20,450, len=100)
SHR1.pred <- predict(SHR1.glm, newdata = data.frame(Quanta = xseq), 
                     type = "response", se.fit = T)
SHR1.pred <- data.frame(x=xseq, fit=SHR1.pred$fit, se.fit=SHR1.pred$se.fit)
head(SHR1.pred)
pp + geom_line(aes(x=x,y=fit), data=SHR1.pred)
pp + geom_ribbon(data=SHR1.pred, 
                 aes(x=x, ymin=fit-se.fit, ymax=fit+se.fit, y=fit), fill="gray")

pf <- pp + geom_ribbon(data=SHR1.pred, 
                 aes(x=x, ymin=fit-se.fit, ymax=fit+se.fit, y=fit), fill="gray") +
  geom_line(aes(x=x,y=fit), data=SHR1.pred) +
  geom_point(size=I(5), colour=I("black"))
pf

q.5   <- -coef(SHR1.glm)[1] / coef(SHR1.glm)[2]
sigma <- 1/coef(SHR1.glm)[2]

thresh.est <- function(p.est, model) {
  q.5   <- -coef(model)[1] / coef(model)[2]
  sigma <- 1/coef(model)[2]
  th <- exp(qnorm(p = p.est,
                  mean = q.5, sd = sigma))
  return(th)
}
thresh.est(.6, SHR1.glm)

th.6 <- (thresh.est(.6,SHR1.glm))
pf + geom_hline(yintercept=0.6, linetype="dotted") + 
  geom_vline(xintercept=th.6, linetype="dotted")







