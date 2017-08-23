rm(list=ls())
#Importing the 1980 US Census (families with 2 or 3 children with mother's age between 21-35):
census<-read.csv("C:\\Users\\jt_an\\Mega\\R\\Data_Analysis_ for_Social_Scientists\\csv\\census80.csv")
'''
workedm: whether the mother works.
weeksm: number of weeks the mother works.
whitem: mother is White.
blackm: mother is Black.
hispm: mother is Hispanic.
othracem: mother is of other race.
sex1st: sex of the first child (0 corresponds to male and 1 to female).
sex2nd: sex of the second child  (0 corresponds to male and 1 to female).
ageq2nd: age in quarters of the second child.
ageq3rd: age in quarters of the third child.
numberkids: number of children in the household. 
'''
str(census)
#Q1:
summary(census)
#Q2: gen a var to indicate a multiple 2nd pregnancy:
census$multiple<-census$ageq2n==census$ageq3rd
summary(census)
3116/(119170+305132)
#Another way:
census$temp[census$ageq2nd==census$ageq3rd]<-1
census$multiple<-0
census$multiple[census$temp==1]<-1
summary(census$multiple)
#Q3: gen a var to indicate if the 1st and 2nd children are the same sex:
census$temp[census$sex1st==census$sex2nd]<-1
census$samesex<-0
census$samesex[census$temp==1]<-1
summary(census$samesex)
#Another way:
census$temp<-(census$sex1st==census$sex2nd)
census$samesex[census$temp==FALSE]<-0
census$samesex[census$temp==TRUE]<-1
summary(census$samesex)
#Q4: labor supply eq
census$threekids<-0
census$threekids[census$numberkids==3]<-1
summary(lm(workedm~threekids+blackm+hispm+othracem,census))
#having a 3rd child reduces the likelihood the mother works by 8.39 p.p.
summary(lm(weeksm~threekids+blackm+hispm+othracem,census))
#having a 3rd child reduces the likelihood the mother works by 3.94 p.p.
#Q5: first stage of fertility
summary(lm(threekids~multiple+blackm+hispm+othracem,census))
#having a multiple pregnancy, the likelihood of having a 3rd child increase by 71.79 p.p. compared to a single pregnancy
summary(lm(threekids~samesex+blackm+hispm+othracem,census))
#having the samesex increase the likelihood by 4.902 p.p. compared to different sexes
#Q6: ivreg using multiple as instrument
#install.packages("AER")
library(AER)
summary(ivreg(workedm~threekids+blackm+hispm+othracem|blackm+hispm+othracem+multiple, data=census))
#having a third child decreases the likelihood of working by 6.41 p.p.
summary(ivreg(workedm~threekids+blackm+hispm+othracem|blackm+hispm+othracem+samesex, data=census))
#having a  third child decreases the likelihood of working by 9.82 p.p.
