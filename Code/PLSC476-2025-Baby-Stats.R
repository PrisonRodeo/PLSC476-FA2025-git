#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                      ####
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PLSC 476 -- September 2, 2025
#
# Review: Basic Statistics
#
# File last modified 8/30/2025
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries...
#
# This code takes a list of packages ("P") and (a) checks 
# for whether the package is installed or not, (b) installs 
# it if it is not, and then (c) loads each of them:

P<-c("moments","psych")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 3-4 times until you get all smileys. :)
#
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# setwd(), or make a project:
#
# setwd("~/Dropbox (Personal)/PLSC 476/")
#
############################################
# First, some code to generate the example
# figures in the slides; see line 168 below
# for the "real data" example...
# 
# It is good practice to set a random number seed
# any time you generate any random numbers, 
# as one does in the simulations that 
# follow.

set.seed(7222009)

# Code for the "many modes" figure:

# Many modes:

x<-seq(-1,1,by=0.01)
nomode <- dunif(x,-0.9,0.9)
uni <- dnorm(x,0,0.2)
b1 <- dnorm(x,-0.5,0.15)
b2 <- dnorm(x,0.5,0.15)
bi <- b1+b2

# Plot... note that "pdf" creates a PDF file
# of the output of whatever commands follow;
# "dev.off()" turns that off:

pdf("Notes and Slides/ManyModes-25.pdf",12,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(x,uni,t="l",lwd=3,xlab="X",yaxt="n",
     ylab="Frequency",main="Unimodal")
par(mar=c(4,2,2,2))
plot(x,bi,t="l",lwd=3,xlab="X",yaxt="n",
     ylab=" ",main="Bimodal")
plot(x,nomode,t="l",lwd=3,xlab="X",yaxt="n",
     ylab=" ",ylim=c(0,0.8),main="No Mode")
dev.off()

# Plots illustrating skewness:

set.seed(7222009)
p<-10
q<-1.5
m<-(p+q)/2
ndraws<-500
left<-rbeta(ndraws,p,q)
noskew<-rbeta(ndraws,m,m)
right<-rbeta(ndraws,q,p)
lskew<-round(skewness(left),3) # skewness...
sskew<-round(skewness(noskew),3)
rskew<-round(skewness(right),3)

pdf("Notes and Slides/EmpiricalMeanMedianModeR-25.pdf",7,5)
par(mfrow=c(3,1))
par(mar=c(4,4,2,2))
# right-skewed:
hist(right,main="Right-Skewed",prob=TRUE,ylim=c(0,5),
     xlab="X",ylab="Density",col="grey",xlim=c(0,1))
lines(density(right),lwd=3)
legend("topright",legend=paste0("Skewness = ",rskew),bty="n")
# symmetrical:
par(mar=c(4,2,2,2))
hist(noskew,main="Symmetrical",prob=TRUE,ylim=c(0,3),
     xlab="X",col="grey",xlim=c(0,1))
lines(density(noskew),lwd=3)
legend("topright",legend=paste0("Skewness = ",sskew),bty="n")
# left-skewed:
par(mar=c(4,2,2,2))
hist(left,main="Left-Skewed",prob=TRUE,ylim=c(0,5.5),
     xlab="X",col="grey",xlim=c(0,1))
lines(density(left),lwd=3)
legend("topleft",legend=paste0("Skewness = ",lskew),bty="n")
dev.off()

# Means/modes/medians and skewness:

p<-10
q<-1.5
m<-(p+q)/2
x<-seq(0,1,by=0.001)
left<-dbeta(x,p,q)
noskew<-dbeta(x,m,m)
right<-dbeta(x,q,p)

pdf("Notes and Slides/MeanMedianMode-25.pdf",6,5)
par(mfrow=c(3,1))
par(mar=c(4,4,2,2))
# right-skewed:
plot(x,right,t="l",lwd=2,main="Right-Skewed",
     xlab="X",ylab="Density")
abline(v=(q/(q+p)),lwd=2,col="red",lty=2) # mean
abline(v=((q-(1/3))/(p+q-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((q-1)/(q+p-2)),lwd=2,col="orange",lty=2) # mode
legend("topright",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
# symmetrical:
par(mar=c(4,4,2,2))
plot(x,noskew,t="l",lwd=2,main="Symmetrical",
     xlab="X",ylab="Density")
abline(v=(m/(m+m))+0.002,lwd=2,col="red",lty=2) # mean
abline(v=((m-(1/3))/(m+m-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((m-1)/(m+m-2))-0.002,lwd=2,col="orange",lty=2) # mode
legend("topright",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
# left-skewed:
plot(x,left,t="l",lwd=2,main="Right-Skewed",
     xlab="X",ylab="Density")
abline(v=(p/(p+q)),lwd=2,col="red",lty=2) # mean
abline(v=((p-(1/3))/(q+p-(2/3))),lwd=2,col="darkgreen",lty=2) # median
abline(v=((p-1)/(p+q-2)),lwd=2,col="orange",lty=2) # mode
legend("topleft",bty="n",legend=c("Mean","Median","Mode"),
       lwd=3,lty=c(2,2,2),col=c("red","darkgreen","orange"))
dev.off()

# Association among variables...
#
# "Shape of relationships" plot:

N <- 400
set.seed(7222009)
X <- runif(N,0,100)
YPmon <- X + runif(N,-40,40)
YPmon <- ifelse(X>30,YPmon+30,YPmon)
YPmon <- ifelse(X>60,-60+4*X+runif(N,-30,30),YPmon)
YPlin <- X + runif(N,-20,20)
YPnonlin <- (log(X)) + runif(N,-0.7,0.7)
YPnonmon <- ((100*X - X^2)/100) + runif(N,-20,20)  

pdf("Notes and Slides/ContinuousRelationships-25.pdf",9,6)
par(mfrow=c(2,4))
plot(X,YPmon,pch=19,main="Monotonic",ylab="Positive")
plot(X,YPlin,pch=19,main="Linear",ylab=" ")
plot(X,YPnonlin,pch=19,main="Non-Linear",ylab=" ")
plot(X,YPnonmon,pch=19,main="Non-Monotonic",ylab=" ")
plot(X,-YPmon,pch=19,main=" ",ylab="Negative")
plot(X,-YPlin,pch=19,main=" ",ylab=" ")
plot(X,-YPnonlin,pch=19,main=" ",ylab=" ")
plot(X,-YPnonmon,pch=19,main=" ",ylab=" ")
dev.off()

# "Strengths of relationships" plot:

Yzero <- (rnorm(N)*50)+200
Yweak <- X + (rnorm(N)*40)
Ystrong <- X + (rnorm(N)*4)

pdf("Notes and Slides/RelationshipStrengths-25.pdf",9,7)
par(mfrow=c(1,3))
plot(X,Yzero,pch=19,main="No Relationship",ylab="Y")
plot(X,Yweak,pch=19,main="Weak Relationship",ylab="Y")
plot(X,Ystrong,pch=19,main="Strong Relationship",ylab="Y")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Get the English Premier League (soccer) data: ####

EPL<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/master/Data/EPL24-25.csv")

# Show the data:

print(EPL)

# Summaries of the EPL data:

summary(EPL)
describe(EPL)

#######
# Create and summarize "points per match":

EPL$PPG <- EPL$Points / EPL$Matches
describe(EPL$PPG)

# Test the hypothesis that PPG is 1.25:

t.test(EPL$PPG,mu=1.25)

#######
# Do London clubs score more?

# Designate London-area clubs:

LACs<-c("Tottenham Hotspur","West Ham United","Chelsea",
        "Crystal Palace","Fulham","Arsenal")
EPL$London<-ifelse((EPL$Team %in% LACs==TRUE),1,0)
table(EPL$London)

# Hypothesis test:

t.test(PPG~London,data=EPL)

#######
# Do teams that score more goals also allow more 
# goals?

with(EPL, cor(Goals,GoalsAgainst))
with(EPL, cor.test(Goals,GoalsAgainst))

# fin