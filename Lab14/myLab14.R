##########
# Basics #
########################
# Print working directory
getwd()
# Set X-values
x=1:20
# Set seed
set.seed(29)
# Generate y
y=4+6*x + rnorm(20,0,5) 
# Call function
T2<- mylsq(x,y)
# plot points
plot(x,y)
# plot least squares line using coefficients
abline(coef = c(T2$b0hat,T2$b1hat),lwd=2,col="blue")
# Check results with linear model and summary
slr=lm(y~x)
summary(slr)
# Using prediction function
xnew<- 15.5
mypred(xnew,3.102214,6.130737)
# Plot prediction
plot(x,y)
abline(coef = c(T2$b0hat,T2$b1hat),lwd=2,col="blue")+points(x=15.5,y=98.12864, pch = 21, bg = "green")
#
## Book problems ###############
# Mostly same as above, besides loading in .csv files
#
#
### OJUICE ###
ojuice <- read.csv(file = "OJUICE.csv")
head(ojuice)
T3<-mylsq(x = ojuice$Pectin, y=ojuice$SweetIndex)
plot(ojuice$Pectin,ojuice$SweetIndex,xlab = "Pectin",ylab = "Sweetness Index")
abline(coef = c(T3$b0hat,T3$b1hat),lwd=2,col="blue")
mypred(x = 300,b0 = T3$b0hat,b1 = T3$b1hat)
#
### DRILLROCK ###
rock <- read.csv(file = "DRILLROCK.csv")
head(rock)
plot(x = rock$DEPTH,y=rock$TIME,xlab = "Drilling Depth",ylab = "Time to Drill 5 ft.")
T3B<-mylsq(x = rock$DEPTH,y=rock$TIME)
T3B
plot(x = rock$DEPTH,y=rock$TIME,xlab = "Drilling Depth",ylab = "Time to Drill 5 ft.")
abline(coef = c(T3B$b0hat,T3B$b1hat),lwd=2,col="blue")
#
### Carbon ###
carbon <- read.csv("CARBON.csv")
head(carbon)
plot(carbon$LabFurnace,carbon$PilotPlant)
T4<-mylsq(x = carbon$LabFurnace,y=carbon$PilotPlant)
plot(x = carbon$LabFurnace,y=carbon$PilotPlant,xlab = "lab furnace",ylab = "Pilot Plant")
abline(coef = c(T4$b0hat,T4$b1hat),lwd=2,col="blue")
T4C<-mysq(x= carbon$LabFurnace,y=carbon$PilotPlant )
T4C
s<-sqrt(T4C$sq)
s
##############################

############################
# Least Squares Regression #
###################################################################
# - Finds the coefficients of the Least Squares Regression Line
#
#
mylsq=function(x,y){
  # Calculations
  ssxx=sum((x-mean(x))^2 )
  ssxy=sum((x-mean(x))*(y-mean(y))) 
  b1hat=ssxy/ssxx
  b0hat=  mean(y)-b1hat*mean(x)   
  return(list(b0hat=b0hat,b1hat= b1hat)) 
}
################################################################

#################################
# Least squares line prediction #
########################################################
# - Given coefficients of Least Squares Regression Line
#   and a desired x-value to predict for; this funciton
#    finds a prediction value for y
mypred=function(x,b0,b1){
  ym=b0+ b1*x
  ym
}
########################################################


######################
# S squared function #
#####################################################
# -Similar to the above Least Squares Regression function
#  mylsq(), yet will continue to calculate the standard deviation 
#   of the regression line. 
mysq=function(x,y){
  n=length(x)
  ssxx=sum((x-mean(x))^2 )
  ssxy=sum((x-mean(x))*(y-mean(y))) 
  b1hat=ssxy/ssxx
  b0hat=  mean(y)-b1hat*mean(x) 
  yhat=b0hat+ b1hat*x 
  ssr=sum((y-yhat)^2) 
  sq= ssr/(n-2)
  return(list(ssr=ssr,sq=sq))
}
#####################################################