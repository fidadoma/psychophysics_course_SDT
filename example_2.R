rm(list=ls())
library(ggplot2); library(psych)
theme_set(theme_bw())
# === === === === === === === === === === === === === === === === === === 
createtable<-function(l) {
  cond<-matrix(l,ncol=2)
  colnames(cond)<-c("Truth","Lie")
  rownames(cond)<-c("I believe","I don't believe")
  cond<-as.table(cond)
  return(cond)
}
# condition 1 -> sober
cond.1<-createtable(c(35,15,10,40))

hit.1<-prop.table(cond.1,2)[1]
fa.1<-prop.table(cond.1,2)[3]
# condition 2 -> booze

cond.2<-createtable(c(40,10,15,35))

hit.2<-prop.table(cond.2,2)[1]
fa.2<-prop.table(cond.2,2)[3]

# values should be:
# hit.1 <- 0.70 # 35/50
# fa.1  <- 0.20 # 10/50
# hit.2 <- 0.80 # 40/50
# fa.2  <- 0.30 # 15/50

# sensitivity
d.prime <- function(hit, fa) {
  return(qnorm(hit) - qnorm(fa))
}
d.prime(hit.1, fa.1)
d.prime(hit.2, fa.2)

  
# bias
bias <- function(hit, fa) {
  cc <- -0.5 * (qnorm(hit) + qnorm(fa))
  return(cc)
}
bias(hit.1, fa.1)
bias(hit.2, fa.2)



