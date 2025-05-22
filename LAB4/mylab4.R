##############################
# .csv file loading function ##############################
# #This will be a different path if in the lab or at home
# dird="E:\\OneDrive\\MATH4753\\DATAxls\\" # 2017
# 
# #my function to read data 
# myread=function(csv){
#   fl=paste(dird,csv,sep="")
#   read.table(fl,header=TRUE,sep=",")
# }
# #EASY WAY TO READ IN FILES
#spruce.df=myread("SPRUCE.csv")#MS pg478
##########################################################





################
# Read in Data ###########################################
# Assign spruce.df to the SPRUCE.csv file using read.csv()
spruce.df = read.csv("SPRUCE.csv")

#Alternate methods of loading in .csv file
#
#with(spruce.df, dput(list(D=BHDiameter,H=Height), 
#file="spruce.dat"))
# Or use 
#spruce.df=read.table(file.choose(),header=TRUE,sep=",")

# Print the working directory to the user
getwd()

# tail() which gives the bottom 6 lines of the data table
tail(spruce.df)
###################
# Plot the data   #####################################
# Set as a pop-up window
windows()
## Main plot ##
# -Height vs. BHDiameter
# -xlim & ylim set to be the max of the data
#
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,max(Height)),xlim=c(0,max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)

# Load in ggplot2
library(ggplot2)
# Set as a pop-up window
windows()
## ggplot of data ##
# -using geom_point()
#
g = ggplot(spruce.df,mapping = aes(x = BHDiameter, y = Height)) + 
  geom_point()
# Print graph to user
print(g)
## Additions to the plot 'g' ##
# -geom_smooth using log(x), x+ I(x^2), or x for the formula
# - assigned colors to examine and compare
#
g = g+ geom_smooth(formula = y~ log(x), method = "lm", col = "steelblue")
g = g + geom_smooth(formula = y ~ x, method = "lm", col = "Black")
g = g + geom_smooth(formula = y~ x+ I(x^2), method ="lm", col = "Red")
# Print graph to user
print(g)
## Add another line using the formula 'poly(x,3)'
g = g + geom_smooth(formula = y~ poly(x,3), method ="lm", col = "green3")
# Shows the graph to user
g



#load s20x library 
library(s20x)

#trendscatter plot Height vs. BHDiameter from data
trendscatter(Height~BHDiameter,f=0.5,data=spruce.df)

# Linear model created from spruce.df 
spruce.lm=lm(Height~BHDiameter,data=spruce.df)

# Summary of linear model
summary(spruce.lm)

# Create residuals from the linear model
height.res=residuals(spruce.lm)

# Create fitted values from the linear mode
height.fit=fitted(spruce.lm)

# set graph to be a pop-up
windows()
# Plot the residuals vs. fitted values 
plot(height.fit,height.res)

# trendscatter residuals vs. fitted values 
trendscatter( height.fit,height.res)

# Plotting the residuals from the linear model
plot(spruce.lm, which =1)

# set graph to be a pop-up
windows()
# The normality chekc and p-values using shapiro-wilk
normcheck(spruce.lm,shapiro.wilk = TRUE)


## Quadratic object using the linear model
# - Creates a quadratic model from the given data
#
quad.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)
# Summarize quadratic model
summary(quad.lm)
# Adds the second term to the "spruce.lm" linear model
add1(spruce.lm,.~.+I(BHDiameter^2))
# Compare The new "spruce.lm" linear model to "quad.lm"
anova(spruce.lm)
anova(quad.lm)
anova(spruce.lm,quad.lm)
# Set This as a Cubic Linear Model
cubic.lm=lm(Height~BHDiameter + I(BHDiameter^2)+I(BHDiameter^3),data=spruce.df)

# Compare the new cubic model the the quadratic with the new term added to it
anova(cubic.lm)
add1(quad.lm,.~.+I(BHDiameter^3))
###################################

###############################################
# Make Scatter plots using the linear models  ###########
# set graph to be a pop-up
windows()
## Main plot ##
# -Height vs. BHDiameter
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,max(Height)),xlim=c(0,max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)
# Print coefficients and names of quadratic linear model
coef(quad.lm)
names(quad.lm)
quad.lm$coef[2]
## Create the Quadratic plot using formula and coefficients
# -basic input way
myplot=function(x){
  0.86089580 +1.46959217*x  -0.02745726*x^2
}
## Create the Quadratic plot using formula and coefficients
# -generalized inputs
myplot=function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
} 

# Create curve of quadratic on plot
curve(myplot, lwd=2, col="steelblue",add=TRUE)

# Plot Quadratic Linear Model
plot(quad.lm, which=1)
# Plot spruce.lm Linear Model
plot(spruce.lm,which=1)
# Normality check of quadratic linear model using shapiro-wilk
normcheck(quad.lm,shapiro.wilk = TRUE)

# Summarize the quadratic linear model
summary(quad.lm)
# Make predictions of 3, 6, & 8 BHDiameter using the quadratic linear model
predict(quad.lm, data.frame(BHDiameter=c(3,6,8)))
# Confidence interval for quadratic linear model
ciReg(quad.lm)
# Make predictions of 15, 18, & 20 BHDiameter using the quadratic linear model
predict(quad.lm, data.frame(BHDiameter=c(15,18,20)))
# Compare Lineaar Models
anova(spruce.lm,quad.lm)
# Set a data set of 15 16,...,22,23,24
data = 15:24
# make predictions of 15:24 BHDiameter using the quadratic linear model
predict20x(quad.lm,data.frame(BHDiameter = data, `I(BhDiameter)^2`=data^2))
# Compare the linear models
anova(quad.lm)
anova(spruce.lm)
# Fitted points of quadratic linear model
height.qfit=fitted(quad.lm)
################################################################


#####################
# MSS, TSS, and RSS #####################################
# -Basic Calculations for MSS, TSS, and RSS, & MSS/TSS
RSS=with(spruce.df, sum((Height-height.qfit)^2))
RSS
MSS = with(spruce.df, sum((height.qfit-mean(Height))^2))
MSS
TSS = with(spruce.df, sum((Height-mean(Height))^2))
TSS
MSS/TSS
#######################################################


###########################
# Cook's on quad.lm model #
cooks20x(quad.lm)
# Remove outlier found by Cook's
quad2.lm=lm(Height~BHDiameter + I(BHDiameter^2) , data=spruce.df[-24,])
# Summarize new model after using Cook's to the previous 
summary(quad2.lm)
summary(quad.lm)
###############################################################################





######### Most of the code are repeats of the code from above

###############################################################################
##  some other code you might need


#The following code plots residuals
windows()
#Plot the data
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,max(Height)),xlim=c(0,max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)

#Make a quadratic model
quad.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)

# Find the coefficients
coef(quad.lm)

#Make a function that produces heights for inputs "x"
myplot=function(x){
  0.86089580 +1.46959217*x  -0.02745726*x^2
}

# add the quadratic to the points 
curve(myplot, lwd=2, col="steelblue",add=TRUE)

#Place segments (residuals) on the plot (except for the 3 largest cooks distances. 18, 21, 24)
with(spruce.df[-c(18,21,24),],segments(BHDiameter, Height, BHDiameter, height.qfit[-c(18,21,24)]) )
with(spruce.df[c(18,21,24),],segments(BHDiameter, Height, BHDiameter, height.qfit[c(18,21,24)], col="Red", lwd=3) )
with( spruce.df, arrows(5,Height[24], BHDiameter[24], Height[24],lwd=2,col="Blue"))
with(spruce.df,text(2,Height[24], paste("Highest Cook's","\n", "distance",sep=" ")))
with(spruce.df, text(BHDiameter,Height, 1:36,cex=0.5,pos=4))
#########################################################################


layout(matrix(1:4,nr=2,nc=2,byrow=TRUE))

#Lets look at where the plots will go
layout.show(4)

#Plot the data
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)
# add the line
abline(spruce.lm)


#make a new plot
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)

abline(spruce.lm)

#make yhat the estimates of E[Height | BHDiameter]
yhat=with(spruce.df,predict(spruce.lm,data.frame(BHDiameter)))
yhat=fitted(spruce.lm)
# Draw in segments making the residuals (regression errors)
with(spruce.df,{
  segments(BHDiameter,Height,BHDiameter,yhat)
})

RSS=with(spruce.df,sum((Height-yhat)^2))

RSS

#make a new plot
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)

#make nieve model
with(spruce.df, abline(h=mean(Height)))
abline(spruce.lm)

#make the explained errors (explained by the model)
with(spruce.df, segments(BHDiameter,mean(Height),BHDiameter,yhat,col="Red"))
MSS=with(spruce.df,sum((yhat-mean(Height))^2))
MSS

# Total  error
#make a new plot
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)

with(spruce.df,abline(h=mean(Height)))
with(spruce.df, segments(BHDiameter,Height,BHDiameter,mean(Height),col="Green"))
TSS=with(spruce.df,sum((Height-mean(Height))^2))
TSS
RSS + MSS
MSS/TSS

summary(spruce.lm)

#obtain coefft values
coef(spruce.lm)

#Calculate new y values given x
predict(spruce.lm, data.frame(BHDiameter=c(15,18,20)))

anova(spruce.lm)

spruce2.lm=lm(Height~BHDiameter + I(BHDiameter^2),data=spruce.df)
summary(spruce2.lm)


### More on the problem
windows()
plot(Height~BHDiameter,bg="Blue",pch=21,cex=1.2,
     ylim=c(0,1.1*max(Height)),xlim=c(0,1.1*max(BHDiameter)), 
     main="Spruce height prediction",data=spruce.df)

yhatt=with(spruce.df,fitted(spruce2.lm))
with(spruce.df,plot(BHDiameter,yhatt,col="Red")
)

sum(residuals(spruce2.lm)^2)
plot(yhatt~BHDiameter,data=spruce.df,type="p")
summary(spruce2.lm)
anova(spruce2.lm)
anova(spruce.lm,spruce2.lm)
MSS
RSS
######################################################


###
# Creates a Piecewise linear model used in Task 7
#########################################################
## piecewise linear model in R
## Model y = b0 + b1x + b2(x-xk)*(x>xk)
## You will need to change the code appropriately
sp2.df=within(spruce.df, X<-(BHDiameter-20)*(BHDiameter>20)) # this makes a new variable and places it within the same df
sp2.df

lmp=lm(Height~BHDiameter + X,data=sp2.df)
tmp=summary(lmp)
names(tmp)
myf = function(x,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0)
}
plot(spruce.df,main="Piecewise regression")
myf(0, coef=tmp$coefficients[,"Estimate"])
curve(myf(x,coef=tmp$coefficients[,"Estimate"] ),add=TRUE, lwd=2,col="Blue")
abline(v=18)
text(18,16,paste("R sq.=",round(tmp$r.squared,4) ))