#################
## Simulations ##
#################
#
########################
# Binomial Simulation  #
################################################################################
#
# Sample:
# c("H","T") Heads or Tails Binomial/Coin Flip Simulation
# prob is 1/2 for each and we set sample size to
# 10 coin flips
#
sample(c("H","T"),size=10,prob=c(1/2,1/2),replace=TRUE)
#
# 1 or 0 Binomial (or any variable), rest same as above.
#
sample(c(1,0),size=10,prob=c(1/2,1/2), replace=TRUE)
#
###############
# Multinomial #
################################################################################
#Boxes
n=5
# Creates a variable "B1" trhough "B5"
B=paste(rep("B",n),1:n,sep="")
# Prints this set to terminal
B
# Sample:
# size sets a sample size of B to be 20
# prob= c(1/5) for sample set of 5 All boxes (categories) equally likely
#
sample(B,size=20,prob=c(1/5,1/5,1/5,1/5,1/5),replace=TRUE)
################################################################################
#
######################
## mybin() function ##
################################################################################
#
# This is a binomial sampling function
#
# User inuputs:
#   -iter = iterations
#   -n    = sample size
#   -p    = probability
# set default values/replaced with scan()?
#
# Function name and inputs
mybin=function(iter=100,n=10, p=0.5){
###
  # Set Matrix "sam.mat" to be an empty matrix
  #   with the dimensions as the inputs where
  #   -Number of columns are iterations
  #   -Number of rows is 'n' sample size
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #
  # Create and empty success vector
  succ=c()
  #
  # Iteration loop of function that samples 1 & 0 as
  #  success or failure, and logs them into the empty
  #  succ vector.
  for( i in 1:iter){
    # The matrix is filled by a sample of the vector c(1, 0) as success/failure
    # with a probability of p & 1-p.
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    # As we define the 1 as a success, summing the matrix
    # will give the quantity of success as the 0's will not
    # change the summation.
    succ[i]=sum(sam.mat[,i])
  }
  # Create a table out of the "success" or succ vector
  succ.tab=table(factor(succ,levels=0:n))
  # Create a barplot to show the "success" vector divided by iterations
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  # Prints the data used for the bar plot success/iterations
  succ.tab/iter
}
# Call mybin()
#   input variations as desired
#
mybin(iter=1000,n=18, p=0.3)
################################################################################


#######################
## mymult() function ##
################################################################################
# This is a multinomial sampling function
#
# - Note: the probability is 1/4 for each outcome
#          as the program chooses 1:k where k = length(p)
#
# Function name and inputs
mymult=function(iter=100,n=10, p=c(1,1,1,1)/4){
###
  # Set Matrix "sam.mat" to be an empty matrix
  #   with the dimensions as the inputs where
  #   -Number of columns are iterations
  #   -Number of rows is 'n' sample size
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  # K, or the number of categories, is defined by the
  #   probability "p" within the user input
  k=length(p)
  #  Create "tab.mat" as an empty matrix to hold the frequencies in each sample
  tab.mat=matrix(NA,nr=k,nc=iter, byrow=TRUE)
  #
  # Iteration loop for sampling 1:k categories
  #   with "probability" prob = p
  for(i in 1:iter){
    # sampling and setting to "sam.mat"
    sam.mat[,i]=sample(1:k,n,replace=TRUE, prob=p)
    # Create a table and set the matrix "tab.mat" to be filled by this table
    tab.mat[,i]=table(factor(sam.mat[,i],levels=1:k))
  }
  # Summation of the frequency matrix
  freq=apply(tab.mat,1,sum)
  # Create Names for the categories 1:k
  names(freq)=1:k
  # Create a barplot to represent the frequency of 1:k samples being chosen
  barplot(freq/(n*iter),col=rainbow(k) )
  # print the frequency table
  tab.mat
}
# Call mymult()
#   input variations as desired
#
mymult(iter=1000,n=10,p=c(1,2,3,4,2)/12)
################################################################################
# Side note:
## R uses a number of built in distributions
## These all begin with r for random sampling
## Use ?distribution to see a more complete list
?rbinom
?rmultinom
?rpois
?rhyper
################################################################################
#
#
#########################
## mysample() function ##
################################################################################
# this is a sampling function that chooses numbers 1 through 10
#
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    # Take a sample of numbers 1 through 10, with replacement
    s=sample(1:10,n,replace=TRUE)
    # Take the sample vector and make it a factor
    sf=factor(s,levels=1:10)
    # Create a bar plot for each iteration
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    # Release the table at time
    Sys.sleep(time)
  }
}
# Call function example
mysample(n=1000, iter=30)

################################################################################
# Some examples of calculation
#4.25
dbinom(2,5,0.25)
dbinom(0:1,5,0.25)
pbinom(1,5,0.25)
#
#4.35
1-pbinom(8,15,1/5)
pbinom(2999,10000,1/5)
