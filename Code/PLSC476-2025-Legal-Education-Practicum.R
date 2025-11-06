#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                      ####
#
# This is some code from PLSC 476, for the class
# occurring November 6, 2025.
#
# It's about law schools.
#
# No, that's not a joke.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load a few necessary R packages [if you haven't
# installed these by now, you should do so, 
# using -install.packages()-]:

library(RCurl)
library(stargazer)
library(usmap)
library(tidyverse)
library(dplyr)

# Set a couple options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also be sure to use -setwd()- to set your
# working directory (that is, where any files
# you create will be kept).
#
setwd("~/Dropbox (Personal)/PLSC 476/Notes and Slides")
#
#####################################################
# Data are from AccessLex, and are for the year
# 2024. They are downloadable here:
#
# https://analytix.accesslex.org/download-dataset
#
# Read in the AccessLex law school data from the 
# course github repo. There are 12 total files;
# we'll pull all of them except the "Transfers"
# file (which is huge and messy):

Admissions <- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/Admissions.csv")
Attrition  <- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/Attrition.csv")
BarPassage <- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/BarPassage.csv")
Curriculum <- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/Curriculum.csv")
Degrees    <- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/Degrees.csv")
Employment <- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/Employment.csv")
Enrollment <- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/Enrollment.csv")
Faculty    <- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/Faculty.csv")
FinancialAid<- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/FinancialAid.csv")
SchoolInformation<- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/SchoolInformation.csv")
StudentExpenses<- read.csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/LawSchools2024/StudentExpenses.csv")

Attrition<-Attrition[c(1:3,5:8)] # pare down "Attrition"

# Now merge all of these into a single dataset. In 
# each file, there is an identifier for each law school,
# called "schoolid" -- that's our friend:

SchoolInformation$CalendarYear<-NULL
DF<-merge(SchoolInformation,Admissions,
          by=c("schoolid","schoolname"),
          all=FALSE)

DF<-merge(DF,Faculty,
          by=c("schoolid","schoolname","CalendarYear"),
          all=FALSE)

DF<-merge(DF,StudentExpenses,
          by=c("schoolid","schoolname","CalendarYear"),
          all=FALSE)

DF<-merge(DF,FinancialAid,
          by=c("schoolid","schoolname","CalendarYear"),
          all=FALSE)

DF<-merge(DF,Attrition,
          by=c("schoolid","schoolname","CalendarYear"),
          all=FALSE)

BarPassage$CalendarYear<-NULL
DF<-merge(DF,BarPassage,
          by=c("schoolid","schoolname"),
          all=FALSE)

Employment$CalendarYear<-Employment$Cohort
DF<-merge(DF,Employment,
          by=c("schoolid","schoolname","CalendarYear"),
          all=FALSE)

rm(Admissions,Attrition,BarPassage,Curriculum,Degrees,
   Employment,Enrollment,Faculty,FinancialAid,
   SchoolInformation,StudentExpenses)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This is a **lot** of data, on more than 400 
# variables. Let's zoom in on a smaller set 
# of key variables:

Schools<-with(DF, data.frame(SchoolID=schoolid,
                        SchoolName=schoolname,
                        State=SchoolState,
                        TermType=TermType.x,
                        SchoolType=SchoolType.x,
                        Applications=NumApps,
                        Offers=NumOffers,
                        Matriculants=NumMatriculants,
                        NFirstYears=TotalFirstYear,
                        PTFirstYears=PTFirstYear,
                        NStudents=NumStudents,
                        MedianUGPA=UGGPA50,
                        MedianLSAT=LSAT50,
                        TotalFaculty=FTFacTotal,
                        FemaleFaculty=FTFacWomen,
                        MinorityFaculty=FTFacMinority,
                        ResTuition=FTResTuition,
                        NonResTuition=FTNonResTuition,
                        NGrads=GradTotal,
                        FirstTimeBarTakers=TotalFirstTimeTakers,
                        FirstTimeBarPassers=totalfirsttimepassers,
                        UltimateBarTakers=UltimateTakers,
                        UltimateBarPassers=UltimatePassers,
                        StatePassRate=(avgstatepasspct)*100))

row.names(Schools)<-Schools$SchoolName

##################################################
# Before we get into the schools, let's make a 
# state-level dataframe, for maps and things,
# by aggregating the school-level data:

states<-group_by(Schools,State)
States<-summarise(states,
                  NSchools=n(),
                  NGrads=sum(NGrads,na.rm=TRUE),
                  BarPassRate=round(mean(StatePassRate,na.rm=TRUE),3))
rm(states)

States$State<-as.character(States$State) # make character

# Add FIPS codes (needed for plotting):

States$abbr<-States$State
States<-merge(usmap::statepop,States,by=c("abbr"),all=TRUE)

# Alaska has no law schools, so they have no 
# school-level data on bar passage at the 
# state level; we'll add that information
# from this page:
#
# https://admissions.alaskabar.org/recent-2024-july

States$State<-ifelse(States$abbr=="AK","AK",States$State)
States$NSchools<-ifelse(States$abbr=="AK",0,States$NSchools)
States$NGrads<-ifelse(States$abbr=="AK",0,States$NGrads)
States$BarPassRate<-ifelse(States$abbr=="AK",64,States$BarPassRate)

# Also: Remove Puerto Rico...

States<-States[States$State!="PR",]

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# State-level maps                            ####
#
# Specify legend colors, low and high:

low<-"yellow"
high<-"navy"

# Number of Law Schools:

pdf("SchoolsMap-25.pdf",10,8)
par(mar=c(1,1,1,1))
plot_usmap(data=States,values="NSchools",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="Number of Law Schools",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()

# Number of 2024 Law Grads:

pdf("LawGradsMap-25.pdf",10,8)
par(mar=c(1,1,1,1))
plot_usmap(data=States,values="NGrads",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="2024 Law School Graduates",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()

# Number of 2019 Law Grads per 100K population (DC is
# excluded because its number is so large that it messes
# up the plot...):

States$GradsPer100K<-States$NGrads/(States$pop_2022/100000)
StatesNoDC<-States[States$State!="DC",]

pdf("LawGradsMap2-25.pdf",10,8)
par(mar=c(1,1,1,1))
plot_usmap(data=StatesNoDC,values="GradsPer100K",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="2024 Law School Graduates Per\n100K Population (D.C. omitted)",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()

# Bar Passage:

pdf("BarPassMap-25.pdf",10,8)
par(mar=c(2,2,2,2))
plot_usmap(data=States,values="BarPassRate",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="Bar Passage Rate",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()

# Is there a relationship between the number of schools
# or grads and the bar passage rate?

r1<-round(with(States,cor(NSchools,BarPassRate,
                          use="complete.obs")),2)

pdf("BarPass-Schools-Scatter-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(States, plot(NSchools,BarPassRate,pch=20,
                  xlim=c(0,20),ylim=c(55,90),
                  xlab="Number of Law Schools",
                  ylab="Bar Passage Rate"))
with(States, text(NSchools,BarPassRate,pos=1,
                  labels=States$State,cex=0.7))
text(15,60,paste0("r = ",r1),cex=1.5)
dev.off()

# Same scatterplot, this time vs. law graduates:

r2<-round(with(StatesNoDC,cor(GradsPer100K,BarPassRate,
                          use="complete.obs")),2)

pdf("BarPass-Grads-Scatter-25.pdf",6,5)
par(mar=c(5,4,2,2))
with(StatesNoDC, plot(GradsPer100K,BarPassRate,pch=20,
                  xlim=c(0,34),ylim=c(55,90),
                  xlab="Number of 2024 Law Graduates\nPer 100,000 Population",
                  ylab="Bar Passage Rate"))
with(StatesNoDC, text(GradsPer100K,BarPassRate,pos=1,
                  labels=StatesNoDC$State,cex=0.7))
text(22,60,paste0("r = ",r2),cex=1.5)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Now let's look at law *schools*...                  ####
#
# Note that there is something strange going on
# with Marquette Law, w.r.t their bar passage numbers.
# We're going to make those missing, so as not to
# throw off our plots, etc.:

Schools$FirstTimeBarPassers<-ifelse(Schools$SchoolName=="Marquette University",
                                    NA,Schools$FirstTimeBarPassers)
Schools$FirstTimeBarTakers<-ifelse(Schools$SchoolName=="Marquette University",
                                    NA,Schools$FirstTimeBarTakers)
Schools$UltimateBarPassers<-ifelse(Schools$SchoolName=="Marquette University",
                                    NA,Schools$UltimateBarPassers)
Schools$UltimateBarTakers<-ifelse(Schools$SchoolName=="Marquette University",
                                   NA,Schools$UltimateBarTakers)

# First generate both overall and first-time
# bar pass rate percentages for each school:

Schools$FTPassRate<-with(Schools,
                          round((FirstTimeBarPassers/FirstTimeBarTakers)*100),3)
Schools$BarPassRate<-with(Schools,
                     round((UltimateBarPassers/UltimateBarTakers)*100),3)

# What is the overall distribution of bar passage
# rates across schools?

pdf("FTBarPass-Histogram-25.pdf",4,5)
par(mar=c(4,4,2,2))
hist(Schools$FTPassRate,col="grey",main="",
     xlab="First-Time Bar Passage Rate",
     xlim=c(20,100),ylim=c(0,60),breaks=16)
abline(v=mean(Schools$FTPassRate,na.rm=TRUE),
       lwd=2,lty=2)
dev.off()

pdf("TotalBarPass-Histogram-25.pdf",4,5)
par(mar=c(4,4,2,2))
hist(Schools$BarPassRate,col="grey",main="",
     xlab="Total Bar Passage Rate",
     xlim=c(20,100),ylim=c(0,60),breaks=8)
abline(v=mean(Schools$BarPassRate,na.rm=TRUE),
       lwd=2,lty=2)
dev.off()

# Q: Which law schools had the highest and 
# lowest bar passage rates in 2024?
#
# Sort and list, first-time...

Schools<-Schools[order(-Schools$FTPassRate),]
head(Schools[,c(2,25)],10,addrownums=FALSE) # top 10
tail(Schools[,c(2,25)],12,addrownums=FALSE) # bottom 12 - take out PSU & Marquette

# and total...

Schools<-Schools[order(-Schools$BarPassRate),]
head(Schools[,c(2,26)],10) # top 10
tail(Schools[,c(2,26)],12) # bottom 12 - PSU + Marquette

# How do those two things looked when they're plotted
# against each other?

BPfit<-lm(BarPassRate~FTPassRate,data=Schools)

pdf("BarPassages-Scatterplot-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(FTPassRate,BarPassRate,pch=20,
                  xlab="First-Time Passage Rate",
                  ylab="Total Passage Rate"))
abline(a=0,b=1,lwd=2) # X=Y line
abline(BPfit,col="red",lwd=2,lty=2)
legend("topleft",bty="n",lty=c(2,1),lwd=2,col=c("red","black"),
       legend=c("Best Fit Line","X = Y"))
text(90,65,paste0("r = ",round(sqrt(summary(BPfit)$r.squared),2)),
     cex=1.5)
dev.off()

# OK, great... So, what are the factors that might
# lead to higher or lower bar passage rates for
# different schools?
#
# Let's start with some basic things, like the
# "quality" of the students, measured in terms of
# LSAT scores and undergraduate GPAs:

LSATfit<-lm(FTPassRate~MedianLSAT,data=Schools)

pdf("LSAT-BarPass-Scatterplot-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(MedianLSAT,FTPassRate,pch=20,
                  ylim=c(30,100),xlab="Median LSAT Score",
                  ylab="First-Year Bar Passage Rate"))
abline(LSATfit,col="red",lwd=2,lty=2)
legend("topleft",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(166,36,paste0("r = ",round(sqrt(summary(LSATfit)$r.squared),2)),
     cex=1.5)
dev.off()

# Now, by median undergraduate GPA ("UGPA"):

UGPAfit<-lm(FTPassRate~MedianUGPA,data=Schools)

pdf("UGPA-BarPass-Scatterplot-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(MedianUGPA,FTPassRate,pch=20,
                  ylim=c(30,100),
                  xlab="Median Undergraduate GPA",
                  ylab="First-Year Bar Passage Rate"))
abline(UGPAfit,col="red",lwd=2,lty=2)
legend("topleft",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(3.75,36,paste0("r = ",round(sqrt(summary(UGPAfit)$r.squared),2)),
     cex=1.5)
dev.off()

# Now, selectivity. We'll measure this as the percentage
# of applicants a school admitted, as a percentage
# of those who applied. So, higher numbers = less
# selective:

Schools$AdmitPercent<-with(Schools,round((Offers/Applications)*100),4)

# Now plot that...

SelFit<-lm(FTPassRate~AdmitPercent,data=Schools)

pdf("Select-BarPass-Scatterplot-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(AdmitPercent,FTPassRate,pch=20,
                  ylim=c(30,100),
                  xlab="Percent of Applicants Admitted",
                  ylab="First-Year Bar Passage Rate"))
abline(SelFit,col="red",lwd=2,lty=2)
legend("bottomleft",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(64,40,paste0("r = ",-round(sqrt(summary(SelFit)$r.squared),2)),
     cex=1.5)
dev.off()

# What about public and private schools? We can 
# guess that it's unlikely that there are important
# bivariate differences, but let's look anyway:

Schools$Type<-ifelse(Schools$SchoolType=="PRI","Private","Public")

PubPri.t<-t.test(FTPassRate~Type,Schools)

pdf("BP-Type-Boxplot-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools, boxplot(FTPassRate~Type,ylim=c(20,100),
                      ylab="First-Year Bar Passage Rate",
                      xlab="School Type"))
text(1.5,28,paste0("t = ",round(PubPri.t$statistic,2),
                 "\nP = ",round(PubPri.t$p.value,3)),
     cex=1.2)
dev.off()

# School size (students and faculty)...

NFit<-lm(FTPassRate~NStudents,data=Schools)
NFacFit<-lm(FTPassRate~TotalFaculty,data=Schools)

pdf("NStud-BarPass-Scatterplot-25.pdf",4,5)
par(mar=c(4,4,2,2))
with(Schools,plot(NStudents,FTPassRate,pch=20,
                  ylim=c(30,100),
                  xlab="Number of Students",
                  ylab="First-Year Bar Passage Rate"))
abline(NFit,col="red",lwd=2,lty=2)
legend("bottomright",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(1700,80,paste0("r = ",round(sqrt(summary(NFit)$r.squared),2)),
     cex=1.5)
dev.off()

pdf("NFac-BarPass-Scatterplot-25.pdf",4,5)
par(mar=c(4,4,2,2))
with(Schools,plot(TotalFaculty,FTPassRate,pch=20,
                  ylim=c(30,100),
                  xlab="Number of Faculty",
                  ylab="First-Year Bar Passage Rate"))
abline(NFacFit,col="red",lwd=2,lty=2)
legend("bottomright",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(150,80,paste0("r = ",round(sqrt(summary(NFacFit)$r.squared),2)),
     cex=1.5)
dev.off()

# Next... COST:
#
# We'll define tuition as the average of resident
# and nonresident tuition; for private / one-price
# schools, this is equal to both. We'll express 
# the cost in thousands of dollars:

Schools$Cost<-with(Schools,(ResTuition+NonResTuition)/2000)

CostFit<-lm(FTPassRate~Cost,data=Schools)

# Plot:

pdf("Cost-BarPass-Scatterplot-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(Cost,FTPassRate,pch=20,
                  ylim=c(30,100),
                  xlab="Tuition Per Year (Thousands of Dollars)",
                  ylab="First-Year Bar Passage Rate"))
abline(CostFit,col="red",lwd=2,lty=2)
legend("bottomright",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(65,40,paste0("r = ",round(sqrt(summary(CostFit)$r.squared),2)),
     cex=1.5)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Bar Passage: Multivariate Models          ####
#
# Now, let's fit some multivariate models to that, and see
# what happens...

FT.fit<-lm(FTPassRate~MedianLSAT+MedianUGPA+AdmitPercent+
                      Type+NStudents+TotalFaculty+
                      Cost,data=Schools)

stargazer(FT.fit,type="latex",model.numbers=FALSE,
          dep.var.caption="Dependent Variable",
          dep.var.labels="First-Time Bar Passage Rate",
          omit.stat=c("f","ser"))

# Finally, context... Most -- but not all -- law school
# graduates take the bar exam in the state where the
# law school is located. That means that some law grads
# face harder bar exams than others... We can get a sense
# of how a school does in relative terms by comparing its
# bar passage numbers to those for the state as a whole.
#
# If we subtract the school's bar passage rate from the
# overall state rate, we see whether a school is over-
# or under-performing relative to that state in general.
#
# We can examine this new number:

Schools$RelativePassRate<-with(Schools,
                               FTPassRate-StatePassRate)

# What does that look like?

pdf("RelativeBarPass-Histogram-25.pdf",6,5)
par(mar=c(4,4,2,2))
hist(Schools$RelativePassRate,col="grey",main="",
     xlab="Relative Bar Passage Rate",
     xlim=c(-40,40),ylim=c(0,40),breaks=16)
abline(v=mean(Schools$RelativePassRate,na.rm=TRUE),
       lwd=2,lty=2)
dev.off()

# Now let's do the same multivariate analysis,
# but for the "relative" passage scores:

FTR.fit<-lm(RelativePassRate~MedianLSAT+MedianUGPA+AdmitPercent+
             Type+NStudents+TotalFaculty+Cost,data=Schools)

stargazer(FT.fit,FTR.fit,type="latex",model.numbers=FALSE,
          dep.var.caption="Dependent Variable",
          dep.var.labels=c("First-Time Bar Passage Rate",
                           "Relative Bar Passage Rate"),
          omit.stat=c("f","ser"))

# Lastly: Predictions!
#
# We can generate predicted bar passage rates (first-time
# and relative) from these models, and then compare the
# predictions against their actual bar passage rates to
# get a sense of which schools are doing well or poorly
# relative to expectations. These differences are 
# called "residuals":

FT.resids<-data.frame(School = rownames(FT.fit$model),
                      RelativePerf = round(FT.fit$residuals,2))

# Lay out the good and the bad...

good<-FT.resids[FT.resids$RelativePerf>0,]
good<-good[order(-good$RelativePerf),]
good$School<-NULL
head(good,10)

bad<-FT.resids[FT.resids$RelativePerf<0,]
bad<-bad[order(bad$RelativePerf),]
bad$School<-NULL
head(bad,10)

# /fin