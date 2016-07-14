############################################################################################
#####################        Long run chi-squared values       #############################
#####################       Created by Corey Chivers, 2009     #############################
############################################################################################

## What do statistical distributions like the Chi-squared really mean?  This script shows
## how the distribution of chi-squared values for a repeated experiment converges on the 
## theoretical distribution.  

## Imagine going out into the field and collecting butterflies and then asking the statistical 
## question: Do my observations fit my hypothesis? In this example, the hypothesis is that 
## colour is determined by simple Mendelian inheritance (3:1 ratio of yellow:silver phenotypes).
## To see how often we expect to reject our hypothesis when it is indeed true, this script 
## simulates the experiment many times, drawing <n> butterflies at a time from a population 
## which exibits a true 3:1 ratio of wing colour.  It then calculates the chi-squared associated 
## with each sample and keeps track of conclusions.

## You can play around with sample size (n) to see how the type I
## error rate behaves under various experimental conditions.




rm(list=ls())
library(animation)
var_ <- new.env()
x<-c(0,1,1,1)			## a true 3:1 phenotype ratio
n<-20				## Sample <n> butterflies at a time
typeI=0 			## a counter for type I errors
N<-500				## Sim <N> samples

ch<-dchisq(seq(0,10,0.01),1) 	## generate the chi-squared probability density function
ch<-cbind(seq(0,10,0.01),ch)


#pdf('chi_squareNY.pdf')

ani.options(interval = 1,title = "Chi-Squared - a long run experiment",
    description = "Chi-Squared example for BIOL-373, created by Corey Chivers",footer=FALSE,
    ani.width=500,ani.height=500,nmax=N+1,outdir=getwd())

ani.start()

layout(matrix(c(2,3,1,1), 2, 2, byrow = TRUE)) 		## format the ploting page
for(i in 1:N)						## do the experiment N times
{
   smp<-sample(x,n,replace=TRUE) 			## draw n butterflies at random
 
   var_$chi<-c(var_$chi,( abs( length( which(smp==0) ) - 5) )^2/5 +  (abs( length( which(smp==1) ) - 15 ) )^2/15 ) 	## calculate Chi-Squared
 															## NOTE: no Yates correction
   if(var_$chi[length(var_$chi)] > qchisq(0.95,1) ) 	# keep track of type-I errors
   {
     typeI=typeI+1
   } 
   TI<-typeI/i
   
   plot(var_$chi,main=c(paste("Silver: ",length( which(smp==0))),paste("Yellow: ",length( which(smp==1) ) )),ylab="Chi-squared",ylim=c(0,15),xlab="Experiment No.",col="blue",pch=19)
 
   legend("topleft",legend=c(paste("Type-I Error Rate: ",formatC(TI, digits=4, format="g", flag="#")),paste("Chi-sqaured = ",formatC(var_$chi[length(var_$chi)], digits=4, format="g", flag="#"))),bg="white")
   abline(qchisq(0.95,1),0,col="red")  			# draw a line at the critical chi-squared value
 
   hist(var_$chi,probability=TRUE,col="blue",xlim=c(0,10),xlab="Chi-squared",main="")  # Plot a histogram of calculated chi-squared values
   frame()
   print(paste('frame',i,'of',N))
}


plot(var_$chi,main=c(paste("Silver: ",length( which(smp==0))),paste("Yellow: ",length( which(smp==1) ) )),ylab="Chi-squared",ylim=c(0,15),xlab="Experiment No.",col="blue",pch=19)
legend("topleft",legend=c(paste("Type-I Error Rate: ",formatC(TI, digits=4, format="g", flag="#")),paste("Chi-sqaured = ",formatC(var_$chi[length(var_$chi)], digits=4, format="g", flag="#"))),bg="white")
abline(qchisq(0.95,1),0,col="red")  							## draw a line at the critical chi-squared value
hist(var_$chi,probability=TRUE,col="blue",xlim=c(0,10),xlab="Chi-squared",main="")  	## Plot a histogram of calculated chi-squared values
lines(ch,col="red",lwd=2)								## Plot the theoretical chi-squared distribution for 1 degree of freedom
ani.stop()
#dev.off() 

############################################################################################
############################################################################################











############################################################################################
################## Plot theoretic chi-squared for df=1,4,10 ################################
############################################################################################
rm(list=ls())

pdf('chi_Theoretical.pdf')

ch<-dchisq(seq(0,24,0.01),1) 	## generate the chi-squared probability density function
ch<-cbind(seq(0,24,0.01),ch)

ch4<-dchisq(seq(0,24,0.01),4) 	## generate the chi-squared probability density function
ch4<-cbind(seq(0,24,0.01),ch4)

ch10<-dchisq(seq(0,24,0.01),10)	## generate the chi-squared probability density function
ch10<-cbind(seq(0,24,0.01),ch10)

plot(ch4,main="",xlab="chi-squared",ylab="density",col="red",type="l",xlim=c(0,24),lwd=3)  
text(3,0.12,"df = 1")
text(9,0.05,"df = 4")
text(15,0.06,"df = 10")

lines(ch,col="blue",lwd=3)
lines(ch10,col="green",lwd=3)

dev.off()
############################################################################################
############################################################################################


