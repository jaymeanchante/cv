#Module III: Decribing Data, Joint and Conditional Distributions

##Summarizing and describing data

#Load the Bihar height and weight data:
bihar_data<-read.csv("Data/Bihar_sample_data.csv")
colnames(bihar_data)

#Take a look at data
summary(bihar_data)
head(bihar_data)

#Subset adult females
bihar_adult_females<-subset(bihar_data,female==1&adult==1)
head(bihar_adult_females)

#Simplest default histogram
pdf("output/2_bihar_female_heaight.pdf")
hist(bihar_adult_females$height_cm)
hide<-dev.off()

#Simples histogram with some customization
pdf("output/2_bihar_female_heaight.pdf")
hist(bihar_adult_females$height_cm,
     main="Adult female height in Bihar",
     freq = F,
     xlab="Height in cm",
     xlim=c(120,200),
     breaks=seq(0,200,by=2),
     col="cornflowerblue", border="darklue")
hide<-dev.off()

#Add a kernel estimation
pdf("output/5_bihar_female_height_kernel.pdf")
hist(bihar_adult_females$height_cm,
     main="Adult female height in Bihar",
     freq = F,
     xlab="Height in cm",
     xlim=c(120,200),
     breaks=seq(0,200,by=2),
     col="cornflowerblue", border="darklue",
     lines(density(bihar_adult_females$height_cm,
                   bw="nrd0", kernel="epanechnikov",
                   na.rm=T),col="black",lwd=3),
     mtext("Source: Double Fortified Salt Baseline Data",side=3,cex=0.75))
hide<-dev.off()

#Combined CDF (between american and Bihar adult females height)
pdf("output/CDF_female_cdf.pdf")
plot(ecdf(bihar_adult_females$height_cm),col="cornflowersblue",
     main="CDF comparison of aduld female height in the US and Bihar",
     xlab="Height in cm", ylab="Density")
plot(ecdf(us_adult_females$height_cm),
     col=scales::alpha("lightcoral",.5),add=T)
legend("left",c("Bihar","US"),
       col=c("cornflowerblue","lightcoral"),
       lwd=18,bty="n")
hide<-dev.off()
