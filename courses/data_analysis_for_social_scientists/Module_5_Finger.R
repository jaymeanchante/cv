#Module 5

##Sampling from an exponential using the inverse sampling method:

#Random draws from uniform distribution
pdf("runiform_inverse_exponential.pdf")
inverse_exponential_cdf<-function(x,lambda)-log(x)/lambda
y<-inverse_exponential_cdf(u,3)
density_y<-density(y)
plot(density_y,type="1",xlim=c(0,2),
     main="PDF of inverse exponential function",
     lwd=3,col="navyblue",xlab="")
hide<-dev.off

#Plot the inverse CDF of the exponential
pdf("uniform_inverse_exponential_qexp.pdf")
y_qexp<-qexp(u,rate=3)
density_y_qexp<-density(y_qexp)
plot(density_y_qexp, type="1",xlim=c(0,2),
     main="PDF of inverse exponential function",
     lwd=3,col="navyblue",xlab="")
hide<-dev.off

#Random draws straight from the exponential
pdf("random_from_exponential.pdf")
y_rexp<-rexp(1000,rate=3)
density_y_rexp<-density(y_rexp)
plot(density_y_rexp,type="l",xlim=c(0,2),
     main="RV drawn from exponential distribution",
     lwd=3,col="darked",xlab="")
hide<-dev.off()

##Poisson simulation
poisson<-numeric(1000000)

lambda<-2
c<-(0.767-0.336/lambda)
beta<-pi/sqrt(3.0*lambda)
alpha<-beta*lambda
k<-(log(c)-lambda-log(beta))

set.seed(20)
u<-runif(1000000,0.1)
x<-(alpha-log((1.0-u)/u)/beta)
n<-floor(x+0.5)
set.seed(42)
v<runif(1000000,0.1)
y<-alpha-beta*x
lhs<-y+log(v/(1.0+exp(y)^2))
rhs<-k+n*log(lambda)-log(factorial(n))

j<-1
for (i in 1:1000000) {
  if(n[i]>=0){
    if(lhs[i]<=rhs[i]){
      poisson[j]<-n[i]
      j<-j+1
    }
  }
}
poisson<-poisson[1:j]

##Sample 25 of 50 States, with and without replacement
states<-read.csv("states.csv")

#Sample 25 without replacemente, 25 with replacement
states_without_replacement<-list(sample(states$state_name,25,replace=F))
states_with_replacement<-list(sample(states$state_name,25,replace=T))

print(states_without_replacement)
print(states_with_replacement)

#Q1: qnorm(pnorm(0.75,lower.tail=TRUE),lower.tail=TRUE)
#Q2: pnorm(qnorm(2.1, lower.tail=TRUE))
#Q3: Malia is applying for med school, she needs to score in the top 20%, last year the exam mean=500 & sd=10.6, what score does she need?
#qnorm(0.8,500,10.6)

##Compute probabilities from normal distribution
x_mean<-2
x_sd<-0.5

x1<-1.2
x1<-1.34
x3<-1.46
x4<-2.08

#Probability it is less than x1?
pnorm(x1,x_mean,x_sd)
#Probability it is between x2 and x3?
pnorm(x3,x_mean,x_sd)-pnorm(x2,x_mean,x_sd)
#Prabability it is less than x4?
pnorm(x4,x_mean,x_sd,lower.tail=F)