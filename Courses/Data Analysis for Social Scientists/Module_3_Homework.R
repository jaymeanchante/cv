#Module 3: Homework
#PART1:

#Preliminaries
rm(list=ls()) #This command removes all existing objects
install.packages("utils")
library("utils")
setwd("C:\\Users\\jt_an\\Documents\\MicroMasters_Data_Economics_Development_Policy\\Data_Analysis_ for_Social_Scientists\\Gender_Stats_csv")

#Getting the data
gender_data<-read.csv("Gender_Stat_Data.csv")
teenager_fr<-subset(gender_data,Indicator.Code=="SP.ADO.TFRT")
rm(gender_data) #Q3

#Data exploring
str(teenager_fr)
head(teenager_fr)
tail(teenager_fr)
mean(teenager_fr$X1975,na.rm=T)
#Q4:"na.rm=T" is necessary because the default is F and it would return a NA
mean(teenager_fr$X1960,na.rm=T) #Q5
sd(teenager_fr$X1960,na.rm=T) #Q5
mean(teenager_fr$X2000,na.rm=T) #Q6
sd(teenager_fr$X2000,na.rm=T) #Q6
#Q6:
fr<-teenager_fr[c(5:60)]
plot(colMeans(fr,na.rm=T))
rm(fr)

#Plotting the mean, and the values for low, middle nad high income countries
#Q8:
low_income<-subset(teenager_fr,Country.Code=="LIC")
middle_income<-subset(teenager_fr,Country.Code=="MIC")
high_income<-subset(teenager_fr,Country.Code=="HIC")

plot_frame<-matrix(NA,5,55)
for (i in 5:59) {
  k<-i-4
  j<-i+1955
  plot_frame[1, k]<-j
  plot_frame[2, k]<-mean(teenager_fr[,i],na.rm=T)
  plot_frame[3, k]<-low_income[,i]
  plot_frame[4, k]<-middle_income[,i]
  plot_frame[5, k]<-high_income[,i]
}

#Q9:
xlim<-range(c(plot_frame[1,]))
ylim<-range(c(plot_frame[2,],plot_frame[3,],plot_frame[4,],plot_frame[5,]))
plot(plot_frame[1,],plot_frame[2,],type="l",col="black", xlim=xlim, ylim=ylim,
     main="Evolution of Adolescent Fertility Rate",
     sub="black=mean,red=low,blue=middle and yellow=high",xlab="year",ylab="rate")
lines(plot_frame[1,],plot_frame[3,],col="red")
lines(plot_frame[1,],plot_frame[4,],col="blue")
lines(plot_frame[1,],plot_frame[5,],col="yellow")

#Q11:
p1<-hist(teenager_fr$X1960)
p2<-hist(teenager_fr$X2000)
plot(p2,col=rgb(1,0,1,1/4),xlim=c(0,250),main="Change in the distribution",
     xlab="values")
plot(p1,col=rgb(0,0,1,1/4),xlim=c(0,250),add=T)
legend("topright",ncol=2,legend=c("2000","1960"),
       fill=c(rgb(1,0,1,1/4),rgb(0,0,1,1/4),text.width=20),png("histogram"))
#Q12: breaks option allow us to choose the number of bins

#Q13:
p1<-hist(teenager_fr$X1960,freq=F,breaks=20)
p2<-hist(teenager_fr$X2000,freq=F,breaks=20)
p1$counts=p1$density
p2$counts=p2$density
p3<-density(teenager_fr$X1960,na.rm=T)
p4<-density(teenager_fr$X2000,na.rm=T)

plot(p2,col=rgb(1,0,1,1/4),xlim=c(0,250),main="Change in the distribution",
                xlab="values",ylab="Density")
plot(p1,col=rgb(0,0,1,1/4),xlim=c(0,250),add=T)
lines(p4,col=rgb(1,0,1,1/4),xlim=c(0,250),lwd=5)
lines(p3,col=rgb(0,0,1,1/4),xlim=c(0,250),lwd=5)
legend("topright",ncol=2,legend=c("2000","1960"),
       fill=c(rgb(1,0,1,1/4),rgb(0,0,1,1/4),text.width=20))
legend("topright",ncol=2,legend=c("2000","1960"),
       fill=c(rgb(1,0,1,1/4),rgb(0,0,1,1/4),text.width=20))
              
#PART 2:
#Q3:
rm(list=ls())
library("utils")
install.packages('plot3D')
library(plot3D)

#Creating the vector x and y:
M<-mesh(seq(0,1,length=100),seq(0,1,length=100))
x<-M$x
y<-M$y
z<-6/5*(M$x+M$y^2)

#Plotting this pdf:
persp3D(x,y,z,xlab='X variable',ylab='Y variable',xlim=c(0,1),
        main='Plotting joint pdf')
#Q11:
Carol<-6/5*(M$x+1/3)
Anna<-6/5*(1/2+M$y^2)
plot(ecdf(Carol),col="red")
plot(ecdf(Anna),col="blue",add=T)