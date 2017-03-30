install.packages("survival")
install.packages("flexsurv")
library(flexsurv)
library('survival')

#data loading
breast_cancer = read.csv("C:/Users/UdyotKumar/Desktop/flex 4/Data Mining 2/Survival Analysis/gbcs.csv", header = TRUE)
breast_cancer = read.csv("C:/Users/UdyotKumar/Desktop/flex 4/Data Mining 2/Survival Analysis/gbcs.csv", header = TRUE)

#Data-preparation
attach(breast_cancer)
grade=as.factor(grade)
hormone=as.factor(hormone)
age = factor ( with 
               ( breast_cancer, 
               ifelse ( ( age <=30 ), 1 ,  ifelse((age <=50), 2, 3)) ) )
age = as.factor(age)
str(breast_cancer)
names(breast_cancer)
time=survtime
event=censdead
X= cbind(age,menopause,hormone,size, grade, nodes, prog_recp, estrg_recp)

#summary statistic
group=age
summary(time)
summary(event)
summary(group)
summary(X)

#Kaplan -Meier non-parametric analysis
kmsurvival = survfit(Surv(time,event)~1)
summary(kmsurvival)
plot(kmsurvival, xlab="Time", ylab="survival probability")

#Kaplan -Meier non-parametric analysis by group
kmsurvival1 = survfit(Surv(time,event)~group)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="survival probability",col=c("red","blue"))
legend('topright', "hormone-Yes" , lty=1, col=c('red'), bty='n', cex=.75)

#Cox-Proportional hazard model
coxph=coxph(Surv(time,event)~age+menopause+hormone+size+nodes+prog_recp+estrg_recp+ grade - 1, method="breslow")
summary(coxph)

coxph2=coxph(Surv(time,event)~age+size+grade+prog_recp+nodes, method="breslow")
summary(coxph2)
plot(survfit(coxph2))

time.bc= coxph(Surv(time,event)~age+size+nodes+prog_recp+estrg_recp, method="breslow" )
time.bc.zph=cox.zph(time.bc, transform = 'log')
time.bc.zph
plot(time.bc.zph)
plot(time.bc.zph[1])
plot(time.bc.zph[2])
plot(time.bc.zph[3])
plot(time.bc.zph[4])
plot(time.bc.zph[5])


require(flexsurv)
install.packages("flexsurv")
library(flexsurv)

#Exponential, weibull, log logistic
s = Surv(time,event)
exponential=survreg(Surv(time,event)~age+menopause+hormone+size+grade+nodes+prog_recp+estrg_recp, dist="exponential")
summary(exponential)
sExp = flexsurvreg(s~as.factor(grade), dist='exponential', data=breast_cancer)
plot(sExp)
sExp = flexsurvreg(s~as.factor(age), dist='exponential', data=breast_cancer)
plot(sExp)

s = Surv(time,event)
weibull=survreg(Surv(time,event)~age+menopause+hormone+size+grade+nodes+prog_recp+estrg_recp, dist="weibull")
summary(weibull)
sWei = flexsurvreg(s~as.factor(grade), dist='weibull', data=breast_cancer)
plot(sWei)

loglogistic=survreg(Surv(time,event)~age+menopause+hormone+size+grade+nodes+prog_recp+estrg_recp, dist="loglogistic")
summary(loglogistic)

# uses log-normal. no log-logistic option
sLog = flexsurvreg(s~as.factor(grade), dist='lnorm', data=breast_cancer)
plot(sLog)
