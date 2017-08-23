#Module 8 homework:
data<-read.csv("C:\\Users\\jt_an\\Documents\\MicroMasters_Data_Economics_Development_Policy\\Data_Analysis_ for_Social_Scientists\\csv\\nlsw88.csv")
head(data)
data_fit<-lm(formula=lwage~yrs_school,data=data)
summary(data_fit)
confint(data_fit, "yrs_school",level=0.9)
sum(residuals(data_fit))
plot(residuals(data_fit))

#Q9:
aggregate(data[,1], list(data$black), mean)
1.746078-1.911614
#B0 is 1.91 (mean_other) and B1 is -0.165536 (mean_black-mean_other)
#Another way:
meanother<-mean(data$lwage[data$lwage==0])
meanblack<-mean(data$lwage[data$lwage==1])
meanother
meanblack-meanother

data_fit2<-lm(formula=lwage~black,data=data)
summary(data_fit2)
-0.16554/0.02744

#Q12
data_fit3<-lm(formula=lwage~yrs_school+ttl_exp,data=data)
summary(data_fit3)
#Q15:
data_fit4<-lm(formula=lwage~I(yrs_school+2*ttl_exp),data=data)
summary(data_fit4)
#Another way to restrict:
data$newvar<-data$yrs_school+2*data$ttl_exp
restricted<-lm(lwage~newvar,data=data)
#Q16:
fit_anova<-anova(data_fit3,data_fit4)
print(fit_anova)
#Another way:
multi<-lm(lwage~yrs_school+ttl_exp,data=data)
summary(multi)
anova_unres<-anova(multi)
restricted<-lm(lwage~newvar,data=data)
anova_res<-anova(restricted)
statistic_test<-(((anova_res$`Sum Sq`[2]-anova_unres$`Sum Sq`[3])/1)
                 /((anova_unres$`Sum Sq`[3])/anova_unres$Df))
statistic_test
pvalue<-df(statistic_test,1,anova_unres$Df[3])
pvalue

library(car)
matrixR<-c(0,-2,1)
linearHypothesis(multi,matrixR)
