##Module 9 Homework:
#First part:
fast<-read.csv("C:\\Users\\jt_an\\Mega\\R\\Data_Analysis_ for_Social_Scientists\\csv\\fastfood.csv")
head(fast)
str(fast)
employ_fit <- lm(empft~state,data=fast)
wage_fit <- lm(wage_st~state,data=fast)
summary(employ_fit)
summary(wage_fit)
4.62863-0.02123
did_fit <- lm(empft2-empft~state,data=fast)
summary(did_fit)
#Second part:
rm(list=ls())
elec <- read.csv("C:\\Users\\jt_an\\Mega\\R\\Data_Analysis_ for_Social_Scientists\\csv\\indiv_final.csv")
head(elec)
sum(elec$difshare[elec$difshare>0])/length(elec$difshare)
library(rdd)
DCdensity(elec$difshare,ext.out = T)

100*sum(elec$difshare>0.4177)/sum(elec$difshare>0)
100*sum(elec$difshare<(-0.193))/sum(elec$difshare<0) #had to do it manually...

elec2 <- elec[which(elec$difshare>=-0.193&elec$difshare<=0.4177),]
elec2$difshare1 <- as.numeric(elec2$difshare>=0)
model1 <- lm(myoutcomenext~difshare1,data=elec2)
model2 <- lm(myoutcomenext~difshare1+difshare,data=elec2)
model3 <- lm(myoutcomenext~difshare1+difshare+difshare*difshare1,data=elec2)
model4 <- lm(myoutcomenext~difshare1+difshare+difshare^2,data=elec2)
model5 <- lm(myoutcomenext~difshare1+difshare+difshare^2+difshare*difshare1+(difshare^2)*difshare1,data=elec2)
model6 <- lm(myoutcomenext~difshare1+difshare+difshare^2+difshare^3,data=elec2)
model7 <- lm(myoutcomenext~difshare1+difshare+difshare^2+difshare^3+difshare*difshare1+(difshare^2)*difshare1+(difshare^3)*difshare1,data=elec2)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
model8<-RDestimate(myoutcomenext~difshare,data=elec,subset=abs(elec$difshare)<=0.5)
summary(model8)
