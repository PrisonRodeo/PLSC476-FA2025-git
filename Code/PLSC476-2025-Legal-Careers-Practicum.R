#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                        ####
#
# This is some code from PLSC 476, for the class
# occurring November 13, 2025.
#
# The topic is the legal profession, and more
# specifically the professional lives of lawyers. 
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load a few necessary R packages [if you haven't
# installed these by now, you should do so, 
# using -install.packages()-]:

library(RCurl)
library(haven)
library(stargazer)
library(usmap)
library(psych)
library(car)
# install.packages("corrplot")
library(corrplot) # <-- probably needs installing
# library(tidyverse)
library(dplyr)

# Set a couple options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also be sure to use -setwd()- to set your
# working directory (that is, where any files
# you create will be kept).
#
setwd("~/Dropbox (Personal)/PLSC 476/Notes and Slides")
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data                                                ####
#
# We'll be using the "After the JD" ("AJD") data, Wave 
# III. These are the third wave of a long-term 
# longitudinal study of attorneys in the U.S. The third 
# wave was collected in late 2012 and early 2013, roughly 
# twelve years after they were first admitted to the
# bar. Details are here:
#
# https://www.americanbarfoundation.org/projects/after-the-jd-a-national-study-of-lawyer-careers/
#
# The Wave III data are available at the ICPSR:
#
# https://www.icpsr.umich.edu/web/ICPSR/studies/35480/datadocumentation
#
# ...and on the course Github repo. The codebook and
# questionnaire are also there. We'll pull the data from
# the latter:

AJD_Full <- read_sav("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/AJD/AJD-Data.sav")

# 565 variables is too many. Subset the data:

AJD<-with(AJD_Full, data.frame(ID=AJD3_ID,
                               Race=CRACE,
                               Female=ifelse(CGENDER==1,1,0),
                               Age=AGE_BRAC,
                               SocialConservativism=CQ061A,
                               EconConservativism=CQ061B,
                               PolParty=CQ060,
                               LSRank=CRANKA,
                               EmplStart=CQ003YEAR,
                               EmplType=CQ004,
                               Position=CQ005,
                               HoursWorkedWeek=CQ021AXBRAC,
                               ProBonoHoursYr=CQ022A,
                               HowOftenInCourt=CQ029,
                               TravelOvernight=CQ030A,
                               BookOfBusiness=CQ041,
                               RemainingEdDebt=CQ057,
                               EdDebt=CQ058XBRAC,
                               Sat.Responsibility=CQ047A,
                               Sat.RecogWork=CQ047B,
                               Sat.OppAdvancement=CQ047E,
                               Sat.Compensation=CQ047F,
                               Sat.WorkControl=CQ047H,
                               Sat.IntellChallenge=CQ047L,
                               Sat.WorkValSociety=CQ047Q,
                               Sat.WorkBalance=CQ047S,
                               LawyerSatisfaction=CQ049A,
                               LawDegGoodInvest=CQ049B1,
                               WouldAttendLawSchool=CQ049B2))

# Now do some recodes/cleaning:

# AJD$Race<-as_factor(AJD$Race)
AJD$Age<-as_factor(AJD$Age)
AJD$PolParty<-as_factor(AJD$PolParty)
AJD$LSRank<-as_factor(AJD$LSRank)
AJD$YearsAtCurrentEmpl<-2013-AJD$EmplStart
AJD$HoursWorked<-as_factor(AJD$HoursWorkedWeek)
AJD$JobType<-Recode(as.character(AJD$EmplType),"1='Solo';2='Law Firm';
                    3='Federal Government';4='State/Local Government';
                    c(5,6,7,8)='Public Interest';9='Education';
                    c(10,11,12)='Industry';else='Other'")
AJD$Debt<-as_factor(AJD$EdDebt)
AJD$EdDebt<-ifelse(is.na(AJD$EdDebt),1,AJD$EdDebt)
AJD$LawyerSatisfaction<-(6-AJD$LawyerSatisfaction)


#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Satisfaction...                                     ####
#
# We'll focus on eight variables that tap different 
# aspects of satisfaction. Let's plot the means
# of those now:

SatVars<-grep("Sat\\.",names(AJD),value=TRUE) # gets the variable names
                                              # in a list...
SatLabels<-c("Level of responsibility you have",
             "Recognition you receive for your work",
             "Opportunities for advancement",
             "Compensation",
             "Control you have over the amount of work you do",
             "Intellectual challenge of your work",
             "Value of your work to society",
             "Balance between personal life and work")
SatMeans<-colMeans(AJD[,SatVars],na.rm=TRUE) # means of the 8 variables

pdf("LawyerSatMeans-25.pdf",7,5)
par(mar=c(4,8,2,2))
dotchart(SatMeans,labels=SatLabels,pch=19,xlim=c(4,7))
dev.off()

# How do these components correlate with 
# each other?

SatCorrs<-cor(AJD[,SatVars],use="complete.obs")

pdf("SatCorrPlot-25.pdf",7,5)
corrplot(SatCorrs,method="number")
dev.off()

# Now, create the additive index...

AJD$SatIndex<-rowMeans(AJD[,SatVars],na.rm=TRUE)

# Plot that...

pdf("SatIndexDensity-25.pdf",7,5)
par(mar=c(4,4,2,2))
plot(density(AJD$SatIndex,na.rm=TRUE),main="",
     xlab="Satisfaction Index",xlim=c(1,7),
     lwd=2)
abline(v=mean(AJD$SatIndex,na.rm=TRUE),lty=2,
       lwd=2,col="red")
legend("topleft",bty="n",lwd=2,lty=2,col="red",
       legend=paste0("Mean = ",
              round(mean(AJD$SatIndex,na.rm=TRUE),2)))
dev.off()

# Next, we'll conduct a factor analysis, to assess
# whether the different satisfaction indicators are
# driven by a common underlying variable, and to
# generate scores that take into account the extent
# to which each component / indicator is related to
# the underlying concept:

SatFA<-factanal(~Sat.Responsibility+Sat.RecogWork+Sat.OppAdvancement+
                  Sat.Compensation+Sat.WorkControl+Sat.IntellChallenge+
                  Sat.WorkValSociety+Sat.WorkBalance,
                data=AJD,factors=1,na.action="na.exclude",
                rotation="varimax",scores="regression")
SatFA

# The factor "scores" -- the estimate of each 
# observation's value on the latent / underlying 
# factor -- are contained in the factor analysis
# object. We can extract them from the object, and
# merge them with the original data:

SatFactorScores<-SatFA$scores
AJD<-cbind(AJD,SatFactorScores)
names(AJD)[names(AJD)=="Factor1"]<-"SatFactorScore"

# Now, what do those look like?

describe(AJD$SatFactorScore)

pdf("SatFactorScoreDensity-25.pdf",7,5)
par(mar=c(4,4,2,2))
plot(density(AJD$SatFactorScore,na.rm=TRUE),main="",
     xlab="Satisfaction Factor Score",xlim=c(-3.5,1.5),
     lwd=2)
abline(v=mean(AJD$SatFactorScore,na.rm=TRUE),lty=2,
       lwd=2,col="red")
legend("topleft",bty="n",lwd=2,lty=2,col="red",
       legend=paste0("Mean = ",
              round(mean(AJD$SatFactorScore,na.rm=TRUE),2)))
dev.off()

# Now compare the factor scores to the eight components,
# and to the additive index:

SatVars2<-c(SatVars,"SatIndex","SatFactorScore")
SatCorrs2<-cor(AJD[,SatVars2],use="complete.obs")

pdf("SatCorrPlot2-25.pdf",8,6)
corrplot(SatCorrs2,method="number")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Direct measures of satisfaction                     ####
#
# Plots:

pdf("LawyerSatisfaction-Histogram-25.pdf",4,5)
par(mar=c(6,4,2,2))
with(AJD,
     barplot(table(LawyerSatisfaction),main="",
          xlab="Satisfaction with Decision\nto Become a Lawyer"))
abline(v=mean(AJD$LawyerSatisfaction,na.rm=TRUE),
       lwd=3,lty=2)
legend("topleft",bty="n",lwd=3,lty=2,
       legend=paste0("Mean = ",
              round(mean(AJD$LawyerSatisfaction,na.rm=TRUE),2)))
dev.off()

pdf("LSGoodInvest-Histogram-25.pdf",4,5)
par(mar=c(6,4,2,2))
with(AJD,
     barplot(table(LawDegGoodInvest),main="",
             xlab="Law Degree Was A Good Investment"))
abline(v=mean(AJD$LawDegGoodInvest,na.rm=TRUE),
       lwd=3,lty=2)
legend("topleft",bty="n",lwd=3,lty=2,
       legend=paste0("Mean = ",
                     round(mean(AJD$LawDegGoodInvest,na.rm=TRUE),2)))
dev.off()

pdf("LSAttendAgain-Histogram-25.pdf",4,5)
par(mar=c(6,4,2,2))
with(AJD,
     barplot(table(WouldAttendLawSchool),main="",
             xlab="Would Attend Law School Again"))
abline(v=mean(AJD$WouldAttendLawSchool,na.rm=TRUE),
       lwd=3,lty=2)
legend("topleft",bty="n",lwd=3,lty=2,
       legend=paste0("Mean = ",
                     round(mean(AJD$WouldAttendLawSchool,na.rm=TRUE),2)))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Compare factor and index scores with the three
# direct measures of satisfaction:

list<-c("SatFactorScore","SatIndex","LawyerSatisfaction",
        "LawDegGoodInvest","WouldAttendLawSchool")
smolDF<-AJD[,list]

pdf("ValidationPlot-25.pdf",7,6)
par(mar=c(3,3,3,3))
pairs.panels(smolDF,density=FALSE,hist.col="grey",lm=TRUE)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Analysis                                            ####
#
# Last, let's compare some analyses of the satisfaction
# measures. We'll fit separate linear regression models
# for the factor score and the "direct" satisfaction 
# measure...
#
# Add a few covariates:

AJD$Black<-ifelse(AJD$Race==1,1,0)
AJD$Latino<-ifelse(AJD$Race==2,1,0)
AJD$NativeAmerican<-ifelse(AJD$Race==3,1,0)
AJD$Asian<-ifelse(AJD$Race==4,1,0)
AJD$OtherRace<-ifelse(AJD$Race==98,1,0)

# Clean up labels, etc.:

AJD<-zap_label(AJD)
AJD<-zap_labels(AJD)
AJD<-zap_formats(AJD)
AJD<-zap_widths(AJD)

FAScore.fit<-lm(SatFactorScore~I(EmplStart-1970)+HoursWorkedWeek+EdDebt+
                  Female+Black+Latino+NativeAmerican+Asian+
                  OtherRace+Age,data=AJD)
summary(FAScore.fit)

DirectSat.fit<-lm(LawyerSatisfaction~I(EmplStart-1970)+HoursWorkedWeek+
                    EdDebt+Female+Black+Latino+NativeAmerican+Asian+
                    OtherRace+Age,data=AJD)
summary(DirectSat.fit)

# Make a nice table:

stargazer(FAScore.fit,DirectSat.fit,type="latex",
          column.labels=c("Factor Score","Direct Measure"),
          model.numbers=FALSE)

# /fin