# Task 2##########################################################

# Sets Layout for 4 graphs 2 on top 2 on bottom
layout(matrix(1:4, nr=2,nc=2))
# Normal Curve Plots
curve(dnorm(x, mean=0,sd=1),xlim=c(-6,25),ylab = "sd=4")
curve(dnorm(x, mean=4,sd=2),xlim=c(2,17),ylab = "sd=2")
curve(dnorm(x, mean=10,sd=4),xlim=c(-30,40),ylab = "sd=10")
curve(dnorm(x, mean=-2,sd=1/2),xlim=c(3,7),ylab = "sd=1/2")
#####################

# In order to find the area between x=10 and 25
#  - seq defines desired x points
xcurve=seq(10,25,length=1000)
# For The Y part of the curve  we use the X points
#  and the data for the curve in a normal distribution
ycurve=dnorm(xcurve,mean=10,sd=4)
# Creates a Polygon and fills it bounded by the given
#   x & y curves
polygon(c(10,xcurve,25),c(0,ycurve,0),col="Red")

# Put in the text with the appropriate area

# Area
prob=pnorm(25,mean=10,sd=4)-pnorm(10,mean=10,sd=4)
prob=round(prob,4)

# Click to paste the text onto the graph
text(locator(1), paste("Area = ", prob, sep=""))

###############################################################










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

# Put in the text with the appropriate area

# Area
prob=pnorm(25,mean=10,sd=4)-pnorm(10,mean=10,sd=4)
prob=round(prob,4)

# Click to paste the text onto the graph
text(locator(1), paste("Area = ", prob, sep=""))

###############  Gamma ################
curve(dgamma(x,shape=1,scale=1),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
ylab="Gamma density", main="Beta=1")
text(locator(1),paste("alpha=",1))
curve(dgamma(x,shape=3,scale=1),xlim=c(0,10),ylim=c(0,1),add=TRUE,lwd=2)
text(locator(1),paste("alpha=",3))
curve(dgamma(x,shape=5,scale=1),xlim=c(0,10),ylim=c(0,1),add=TRUE, col="Blue",lwd=2)
text(locator(1),paste("alpha=",5))

################### Chi -sq ######### alpha = df/2, beta=2
curve(dchisq(x,df=2),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
ylab="Chisq density", main="df=2,5")
text(locator(1),paste("df=",2))
curve(dchisq(x,df=5),xlim=c(0,10),ylim=c(0,1),col="Blue",lwd=2,
ylab="Chisq density",add=TRUE)

## Beta
curve(dbeta(x,shape1=2,shape2=3),xlim=c(0,1))

#P( 0.2<=Y<=0.6)
