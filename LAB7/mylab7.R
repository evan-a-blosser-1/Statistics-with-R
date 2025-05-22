################
## mychisim() ##
################################################################################
# =>This is a Chi-square statistic simulated as a .R function,
#     and uses the rnorm() function to make a sample from
#     population data.
# =>Returns: 'w' or shi-square
#
# =>Variables:
#       - data1.mat ; Data matrix used to hold rnorm() sample data
#       - ssq1      ; "S" squared which is the sample variance
# =>User Inputs:
#       - n1     ; sample size desired
#       - sigma1 ; Standard Deviaton of the population
#                     (sigma1^2 is the pop variance)
#       - mean1  ; Mean of the population
#       - iter   ; Iterations of the function to be carried out, or
#                   "iter" amount of samples of size "n1"
#       - ymax   ; User defined to fit the y-axis of the output
#                     graphs for analysis
#
mychisim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,...){
  # Generate the "iter" amount of samples using rnorm
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)
  # Define dimension of data matrix by user input
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
  # Define ssq1
  ssq1=apply(data1.mat,2,var)
  # chi-sqaure Statistic
  #  denoted as 'w'
  w=(n1-1)*ssq1/sigma1^2
  # Histogram
  #   - User Input Adaptive labels
  hist(w,freq=FALSE, ylim=c(0,ymax),
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  # Creates density plot lines on-top of histogram
  lines(density(w),col="Blue",lwd=3)
   # Creates theoretical curve on-top of histogram
  curve(dchisq(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3)
  # Adaptive Title:
  # mathematical annotation -see ?plotmath
  title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2))
  # Legend placed using locater/coordinates if in .rmd
  legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)
  ### Function Return ###
  return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq"))
}
# Function examples:                 ###########################################
windows()                            #
chisq=mychisim(iter=10000,ymax=0.15) #
######################################

################
## mychisim2() ##
################################################################################
# =>This is a Chi-square statistic simulated as a .R function,
#     and uses the rnorm() function to make samples from 2
#     sets of population data.
# =>Returns: 'w' or shi-square
#
# =>Variables: (1 or 2 denotes the population; ie data2.mat)
#       - data1.mat ; Data matrix used to hold rnorm() sample data
#       - ssq1      ; "S" squared which is the sample variance
# =>User Inputs:
#       - n1     ; sample size desired
#       - sigma1 ; Standard Deviaton of the population
#                     (sigma1^2 is the pop variance)
#       - mean1  ; Mean of the population
#       - iter   ; Iterations of the function to be carried out, or
#                   "iter" amount of samples of size "n1"
#       - ymax   ; User defined to fit the y-axis of the output
#                     graphs for analysis
#
mychisim2<-function(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000,ymax=0.07,...){
  # Generate the 1st set of "iter" amount of samples using rnorm
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)
  # Generate the 2nd set of "iter" amount of samples using rnorm
  y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
  # Define dimension of data matrix by user input
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
  data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
  # Define 'S' Squared
  ssq1=apply(data1.mat,2,var)
  ssq2=apply(data2.mat,2,var)
  # pooled s squared 'ssq1' & 'ssq2'
  spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2)
  # chi-sqaure Statistic
  #  denoted as 'w'
  #  -sigma1=sigma2
  w=(n1+n2-2)*spsq/(sigma1^2)
  # Histogram
  #   - User Input Adaptive labels
  hist(w,freq=FALSE, ylim=c(0,ymax),
       main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",chi^2)),
       xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
  # Creates density plot lines on-top of histogram
  lines(density(w),col="Blue",lwd=3)
  # Creates theoretical curve on-top of histogram
  curve(dchisq(x,n1+n2-2),add=TRUE,col="Red",lty=2,lwd=3)
  # Adaptive Title:
  # mathematical annotation -see ?plotmath
  title=expression(chi^2==frac((n[1]+n[2]-2)*S[p]^2,sigma^2))
  # Legend placed using locater/coordinates if in .rmd
  legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)
  ### Function Return ###
  return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq"))
}
# Function examples:  ##########################################################
windows()                   #
csq1= mychisim2(iter=10000) #
#############################

################
## myTsim2()  ##
################################################################################
# =>This is a T statistic simulated as a .R function.
#     Uses the rnorm() function to make samples from 2
#     sets of population data.
# =>Returns: 'w' or shi-square
#
# =>Variables: (1 or 2 denotes the population; ie data2.mat)
#       - data.mat  ; Data matrix used to hold rnorm() sample data
#       - ssq       ; "S" squared which is the sample variance
# =>User Inputs:
#       - n      ; sample size desired
#       - sigma  ; Standard Deviaton of the population
#                     (sigma1^2 is the pop variance)
#       - mean   ; Mean of the population
#       - iter   ; Iterations of the function to be carried out, or
#                   "iter" amount of samples of size "n1"
#       - ymax   ; User defined to fit the y-axis of the output
#
myTsim2<-function(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000,ymax=0.5,...){
  # Generate the 1st set of "iter" amount of samples using rnorm
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)
  # Generate the 2nd set of "iter" amount of samples using rnorm
  y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
  # Define dimension of data matrix by user input
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
  data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
  # Define 'S' Squared & ybar as mean
  ssq1=apply(data1.mat,2,var)
  ybar1= apply(data1.mat,2,mean)
  ssq2=apply(data2.mat,2,var)
  ybar2=apply(data2.mat,2,mean)
  # pooled s squared 'ssq1' & 'ssq2'
  spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2)
  # chi-sqaure Statistic
  #  denoted as 'w'
  #  -sigma1=sigma2
  #  - ybar differenced with mean
  w=((ybar1-ybar2)-(mean1-mean2))/sqrt(spsq*(1/n1+1/n2))
  # Histogram
  #   - User Input Adaptive labels
  hist(w,freq=FALSE, ylim=c(0,ymax),
       main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",T)),
       xlab=paste(" T Statistic",sep=""), las=1)
  # Creates density plot lines on-top of histogram
  lines(density(w),col="Blue",lwd=3)
  # Creates theoretical curve on-top of histogram
  curve(dt(x,n1+n2-2),add=TRUE,col="Red",lty=2,lwd=3)
  # Adaptive Title:
  # mathematical annotation -see ?plotmath
  title=expression(T==frac((bar(Y)[1]-bar(Y)[2])-(mu[1]-mu[2]),S[p]*sqrt(frac(1,n[1])+frac(1,n[2]))))
  # Legend placed using locater/coordinates if in .rmd
  legend(Locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)
  ### Function Return ###
  return(list(w=w,summary=summary(w),sdw=sd(w),fun="T"))
}
# Function examples:  ##########################################################
windows()             #
myTsim2(iter=10000)   #
#######################


################
## myFsim2()  ##
################################################################################
# =>This is a F statistic simulated as a .R function.
#     Uses the rnorm() function to make samples from 2
#     sets of population data.
# =>Returns: 'w' or shi-square
#
# =>Variables: (1 or 2 denotes the population; ie data2.mat)
#       - data.mat  ; Data matrix used to hold rnorm() sample data
#       - ssq       ; "S" squared which is the sample variance
# =>User Inputs:
#       - n      ; sample size desired
#       - sigma  ; Standard Deviaton of the population
#                     (sigma1^2 is the pop variance)
#       - mean   ; Mean of the population
#       - iter   ; Iterations of the function to be carried out, or
#                   "iter" amount of samples of size "n1"
#       - ymax   ; User defined to fit the y-axis of the output
#
myFsim2<-function(n1=10,n2=14,sigma1=3,sigma2=2,mean1=5,mean2=10,iter=1000,ymax=0.9,...){
  # Generate the 1st set of "iter" amount of samples using rnorm
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)
  # Generate the 2nd set of "iter" amount of samples using rnorm
  y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
  # Define dimension of data matrix by user input
  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE)
  data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
  # Define 'S' Squared
  ssq1=apply(data1.mat,2,var)
  ssq2=apply(data2.mat,2,var)
  # chi-sqaure Statistic
  #  denoted as 'w'
  #  -sigma1=sigma2
  #  -spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2) # pooled s squared
  w=ssq1*sigma2^2/(ssq2*sigma1^2)
  # Histogram
  #   - User Input Adaptive labels
  hist(w,freq=FALSE, ylim=c(0,ymax),
       main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",F)),
       xlab=paste("F Statistic",sep=""), las=1)
  # Creates density plot lines on-top of histogram
  lines(density(w),col="Blue",lwd=3)
  # Creates theoretical curve on-top of histogram
  curve(df(x,n1-1,n2-1),xlim=c(0,6),add=TRUE,col="Red",lty=2,lwd=3)
  # Adaptive Title:
  # mathematical annotation -see ?plotmath
  title=expression(F==frac(s[1]^2,s[2]^2)*frac(sigma[2]^2,sigma[1]^2))
  # Legend placed using locater/coordinates if in .rmd
  legend(6,0.5,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)
  ### Function Return ###
  return(list(w=w,summary=summary(w),sd=sd(w),fun="F"))
}
# Function examples:  ##########################################################
myFsim2(iter=10000)   #
#######################







