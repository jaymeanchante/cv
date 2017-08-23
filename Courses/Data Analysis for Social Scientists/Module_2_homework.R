my_binomial<-rbinom(1000,8,0.5)
k<-c(0:8)
cdf<-rep(0.0, times=9)

for (i in 0:8) {
  j <- i+1
  cdf[j]<-sum(my_binomial<=i)/1000

}
plot(k,cdf)


library("scatterplot3d")
x<-rbinom(1000,8,0.5)
y<-rbinom(1000,8,0.2)

k<-c(rep(0:8,times=9),rep(0:8,each=9))
k<-matrix(k, ncol=2,byrow=FALSE)
z<-k[,1]+k[,2]

cdf<-rep(0.0,times=81)

for (i in 1:81) {
  cdf[i]<-sum(x+y<=z[i])/1000
}
scatterplot3d(k[,1],k[,2],cdf,highlight.3d=TRUE,col.axis="blue",
              main="Plotting bivariate CDF", pch=20,ylab="values of Y",
              xlab="values of x",zlab="values of x+y")
