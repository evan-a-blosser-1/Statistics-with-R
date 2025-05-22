#########################################
# Code written and installed in package #
#   for Task 7
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma),col="purple")
  # Create Curves
  xcurve=seq(-100,a,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  # Create Polygon Volume
  polygon(c(-100,xcurve,a),c(0,ycurve,0),col="cyan")
  # Probability
  prob=pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  text(x=a,y=.05, paste("Area = ", prob, sep=""))
}
###############################################################################


####################
# lab6.R Commented #
##############################################################
# Sets Layout for 4 graphs 2 on top 2 on bottom
layout(matrix(1:4, nr=2,nc=2))
# Shows the layout that was made, with numbers 1 through
#  4 to designate each
windows;layout.show(4)
#####################
# Plots a curve with X-limits -10 and 30, with a Normal distribution
#  -w/ mean = 10, Standard Dev. = 4
curve(dnorm(x, mean=10,sd=4),xlim=c(-10,30))
#  -w/ mean = 10, Standard Dev. = 8
curve(dnorm(x, mean=10,sd=8),xlim=c(-10,30))
#  -w/ mean = 5, Standard Dev. = 4
curve(dnorm(x, mean=5,sd=4),xlim=c(-10,30))
#  -w/ mean = 10, Standard Dev. = 2
curve(dnorm(x, mean=10,sd=2),xlim=c(-10,30))
################
#
#####################
# Plots a curve with X-limits -10 and 30, with a Normal distribution
#  -w/ mean = 10, Standard Dev. = 4
curve(dnorm(x, mean=10,sd=4),xlim=c(-10,30))
#
# In order to find the area between x=10 and 25
#  - seq defines desired x points
xcurve=seq(10,25,length=1000)
# For The Y part of the curve  we use the X points
#  and the data for the curve in a normal distribution
ycurve=dnorm(xcurve,mean=10,sd=4)
# Creates a Polygon and fills it bounded by the given
#   x & y curves
polygon(c(10,xcurve,25),c(0,ycurve,0),col="Red")
# Area based probability, this is where the inequality is defined
#   as the bounds to the polygon
prob=pnorm(25,mean=10,sd=4)-pnorm(10,mean=10,sd=4)
prob=round(prob,4)
#
# Uses mouse/click to add text of Area of probability
text(locator(1), paste("Area = ", prob, sep=""))
###############################################################
#
###############  Gamma ################
# Curve based of Gamma Distribution
curve(dgamma(x,shape=1,scale=1),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
ylab="Gamma density", main="Beta=1")
# Text Click Paste
text(locator(1),paste("alpha=",1))
# Curve based of Gamma Distribution
curve(dgamma(x,shape=3,scale=1),xlim=c(0,10),ylim=c(0,1),add=TRUE,lwd=2)
# Text Click Paste
text(locator(1),paste("alpha=",3))
# Curve based of Gamma Distribution
curve(dgamma(x,shape=5,scale=1),xlim=c(0,10),ylim=c(0,1),add=TRUE, col="Blue",lwd=2)
# Text Click Paste
text(locator(1),paste("alpha=",5))

################### Chi -sq ######### alpha = df/2, beta=2
# Curve Based off Chi-Square distribution
curve(dchisq(x,df=2),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
ylab="Chisq density", main="df=2,5")
# Text Click Paste
text(locator(1),paste("df=",2))

curve(dchisq(x,df=5),xlim=c(0,10),ylim=c(0,1),col="Blue",lwd=2,
ylab="Chisq density",add=TRUE)

## Beta
# Curve Based off Beta Distribution
curve(dbeta(x,shape1=2,shape2=3),xlim=c(0,1))

#P( 0.2<=Y<=0.6)
