library(Intro2R)
ddt
L <- ddt$LENGTH
L
n<-length(L)
alpha <- 0.05

chiL <- qchisq(1-alpha/2,n-1)
chiU <- qchisq(alpha/2,n-1)

cinterval <- c((n-1)*var(L)/chiL,(n-1)*var(L)/chiU)
cinterval
