============Data-driven Analysis and Prediction of COVID-19 Infection in Southeast Asia using A Phenomenological Model==============================================
#INDONESIA

Indo<-read.table('Indonesia.txt',header=TRUE)
g=function(t,Y,k,t0) Y*(1+exp(-k*(t-t0)))^(-1)
f=function(t,Y,k,t0,m) Y*(1+exp(-k*(t-t0)))^(-1/m)
plot(jitter(IndoKum,4) ~ jitter(t,2), data=Indo,xlab="Days",ylab="Cumulative",main="INDONESIA",col="blue",ylim=c(0,1600000))
plot(jitter(Indonesia,3) ~ jitter(t,2), data=Indo,xlab="Days",ylab="Positive Case",main="INDONESIA",col="blue",type="o")

curve(g(x, Y=1511712,k=0.035,t0=344.98), add=TRUE, lty=2)
res.g=nls(IndoKum ~ g(t,Y,k,t0), start=c(Y=1511712,k=0.035,t0=344.98),data=Indo,trace=TRUE, control=nls.control(printEval=TRUE, minFactor=2^-24, warnOnly=TRUE))
res.g
summary(res.g)
curve(g(x, Y=2340000,k=0.01538,t0=349.4), add=TRUE,col="red",lwd=7)
AIC(res.g)

curve(f(x, Y=2340000,k=0.01538,t0=349.4, m=1), add=TRUE, lty=2)
res.f=nls(IndoKum ~ f(t,Y,k,t0,m), start=c(Y=2411000,k=0.01521,t0=353, m=1),data=Indo,trace=TRUE, control=nls.control(printEval=TRUE, minFactor=2^-24, warnOnly=TRUE))
res.f
summary(res.f)
AIC(res.f)
curve(f(x, Y=1779000,k=0.02876,t0=362.1, m=2.205), add=TRUE, col="green",lwd=3)
legend("topleft",c("Actual","Initial Assumption","logistic Growth Curve","Richards Curve"),bty="o",lwd=c(2,2,2,2),lty=c(3,2,1,1),col=c("blue","black","red","green"))

==========================================================================
Indo<-read.table('Indonesia.txt',header=TRUE)
Indo
attach(Indo)
t=c(1:670)
g=2340000*(1+exp(-0.01538*(t-349.4)))^(-1)
f=1779000*(1+exp(-0.02876*(t-362.1)))^(-1/2.205)
dd<-data.frame(
  Date=seq(as.Date("2020-03-02"), as.Date("2021-12-31"), by="day"),
  Cumulative = g)
ff<-data.frame(
  Date=seq(as.Date("2020-03-02"), as.Date("2021-12-31"), by="day"),
  latif = f)
ee<-data.frame(
  Date=seq(as.Date("2020-03-02"), as.Date("2021-03-31"), by="day"),
  Cum = IndoKum)
with(dd, plot(Date, Cumulative, xaxt="n",type="l",col="red",lwd=2,main="INDONESIA",ylim=c(0,2500000)))
with(ff, lines(Date, latif, xaxt="n",type="l",col="green",lwd=2))
with(ee, lines(Date, Cum, xaxt="n",type="p",col="blue",lwd=3))
axis.Date(1, at=seq(min(dd$Date), max(dd$Date), by="day"), format="%Y-%m-%d")
legend("bottomright", legend=c("Actual", "Prediction LGM","Prediction RM"),
       col=c("blue", "red","green"), lty=c(3,1,1), lwd=2)