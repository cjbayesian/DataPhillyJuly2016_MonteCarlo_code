############################################################################################
########  Long run regression estimates to visualize regression standard error     #########
#################         Created by Corey Chivers, 2009                 ###################
############################################################################################



rm(list=ls())
#library(animation)
true_a<-0	## Sets up the true populations parameters
true_b<-0	## intercept = 0, slope = 1

track <- new.env()
data <- new.env()
n<-20		## Measure n individuals at a time
N=200		## do the sim N times

data$independent<-1:n		## Where the samples will be held
data$y<-1:n


sample<-function() ## This function samples from the true relationship between independent and y.
{
    for(i in 1:n)
    {
        data$y[i]<-true_a + true_b * data$independent[i] + rnorm(1,0,4)  ## rnorm distributes the error term normally with variance = 4
    }
}

pdf('reg_se.pdf')
#ani.options(interval = 1,title = "Regression Standard Error - a long run experiment",
#    description = "Regression Standard Error example for BIOL-373, created by Corey Chivers",footer=FALSE,nmax=N,
#    ani.width=500,ani.height=500,outdir=getwd())

#ani.start()

par(mfrow=c(2,2))	## format the plot page
t1 <- 0
for(j in 1:N)		## Do the experiment 200 times
{
    sample()		## take a new sample
    regr<-lm(y~independent,data=data)	## fit the regression
    s<-summary(regr)

    plot(data$independent,data$y,ylim=c(-15,15),xlim=c(0,20),xlab="independent",ylab="y",main=paste("Sample from y=",true_b,"x + ",true_a," + error"))
    abline(regr)	## plot the regression line over the sample data
    abline(true_a,true_b,lty=2)
    legend('topleft',legend=c('Regression Line','True Relationship'),lty=c(1,2))
    track$intercept<-c(track$intercept,regr$coefficients[1])	## Keep track of the estimates for the histograms
    track$slope<-c(track$slope,regr$coefficients[2])

    ###### F #######
    ss_reg<-sum(  ((regr$coefficients[1]+regr$coefficients[2]*data$independent)-mean(data$y) )^2  )
    ss_error<-sum(  ((regr$coefficients[1]+regr$coefficients[2]*data$independent)-data$y )^2  )

    track$F = c(track$F, (ss_reg/1) / (ss_error/(n-1)) )
    hist(track$F,xlim=c(0,15),main="F ratio",xlab="F ratio",probability=TRUE)
    curve(df(x,1,n-1),add=TRUE)
    crit_F <- qf(0.95,1,n-1) 
    abline(v=crit_F)
    F <- (ss_reg/1) / (ss_error/(n-1))
    abline(v=F,lty=2,col='blue',lwd=2)
    yloc <- 0.75 * par("yaxp")[2]
    if(crit_F <= F){t1 <- t1+1}
    t1_rate <- t1/j
    text(10,yloc,paste("Type I rate:",round(t1_rate,3)))
    #################
    
    SE_a<-dnorm(seq(-10,10,0.1),true_a,s$coefficients[1,2])  ##generate SE_a distribution
    SE_a<-cbind(seq(-10,10,0.1),SE_a)

    SE_b<-dnorm(seq(0,2,0.01),true_b,s$coefficients[2,2])    ##generate SE_b distribution
    SE_b<-cbind(seq(0,2,0.01),SE_b)

    hist(track$intercept,xlim=c(-10,10),main="histogram of intercept estimates",xlab="intercept estimates",probability=TRUE)
    abline(v=regr$coefficients[1],lty=2,col='blue',lwd=2)
    #lines(SE_a)
    hist(track$slope,xlim=c(-2,2),main="histogram of slope estimates",xlab="slope estimates",probability=TRUE)
    abline(v=regr$coefficients[2],lty=2,col='blue',lwd=2)
    #lines(SE_b)
	
    print(paste(j,'out of',N))
}
#ani.stop()
dev.off()
############################################################################################
############################################################################################


