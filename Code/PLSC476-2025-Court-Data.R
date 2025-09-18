#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PLSC 476: Empirical Legal Studies (Fall 2025)   ####
#
# Code for September 18: Finding and working with
# court data.
#
# Set working directory (uncomment / change as necessary):
#
# setwd("~/Dropbox (Personal)/PLSC 476")
#
# Loading packages:

P<-c("RCurl","haven","DescTools","tidyverse","lubridate")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^^^ code 6-7 times until you get all smileys. :)
#
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Supreme Court Database...                         ####
#
# 1a. "Modern" Supreme Court database (OT 1946-2024)...
#
# Case-Centered data, organized by docket number:

unlink("SCDBData/*.*", recursive = TRUE)

url<-"https://scdb.la.psu.edu/?jet_download=c29d10b22077fe9fbb5a80a624a4ab1bd50ab536"
path<-"SCDBData/modcase.zip"  # Local file name

download.file(url,path,mode="wb")
unzip("SCDBData/modcase.zip",exdir="SCDBData")
filename<-dir("SCDBData")[2]

scdb.cc<-read.csv(paste0("SCDBData/",filename))
rm(filename,path,url)

# Data structure (not shown):

head(scdb.cc[1:5])

#=-=-=-=-=-=-=-=-=
# Justice-Centered data, organized by docket number:

url<-"https://scdb.la.psu.edu/?jet_download=f63edb5d812a973b488eb48e42935b673c8987b3"
path<-"SCDBData/modjustice.zip"  # Local file name

download.file(url,path,mode="wb")
unzip("SCDBData/modjustice.zip",exdir="SCDBData")
filename<-dir("SCDBData")[4]

scdb.jc<-read.csv(paste0("SCDBData/",filename))
rm(filename,path,url)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# "Legacy" data: OT1791-1945:
#
# Case-centered:

url<-"https://scdb.la.psu.edu/?jet_download=59bfe0a9fb9418b894188b0b75600abd94e6c7e6"
path<-"SCDBData/legcase.zip"  # Local file name

download.file(url,path,mode="wb")
unzip("SCDBData/legcase.zip",exdir="SCDBData")
filename<-dir("SCDBData")[6]

legacy.cc<-read.csv(paste0("SCDBData/",filename))
rm(filename,path,url)

# Justice-centered:

url<-"https://scdb.la.psu.edu/?jet_download=6648af3dd54e0448160e7162044b7275d8f7a3f0"
path<-"SCDBData/legjustice.zip"  # Local file name

download.file(url,path,mode="wb")
unzip("SCDBData/legjustice.zip",exdir="SCDBData")
filename<-dir("SCDBData")[8]

legacy.jc<-read.csv(paste0("SCDBData/",filename))
rm(filename,path,url)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# What can we do with all this?                       ####
#
# Modern, case-centered: Plot the number of Right to Privacy
# cases in each term, OT1946-2024:

privacy<-table(scdb.cc[scdb.cc$issueArea==5,]$term)

pdf("Notes and Slides/PrivacyCasesByTerm25.pdf",6,5)
par(mar=c(4,4,2,2))
plot(privacy,xlab="Term",ylab="Frequency")
dev.off()

# Modern, justice-centered: Plot the proportion of liberal
# votes cast by each justice in cases involving the
# First Amendment:

FirstA<-with(scdb.jc[scdb.jc$issueArea==3,],
             prop.table(xtabs(~justiceName+(direction-1)),1))
FirstA<-FirstA[order(FirstA[,2]),] # sort

pdf("Notes and Slides/FirstAmdtLibVoting25.pdf",7,5)
par(mar=c(4,4,2,2))
barplot(FirstA[,2]*100,horiz=TRUE,las=2,cex.names=0.5,
        xlim=c(0,100),xlab="Liberal Voting Percentage")
dev.off()

# Legacy, case-centered: Histogram of the number of cases
# involving Native American tribes as litigants (either 
# as petitioner or respondent):

NATdata<-legacy.cc[(legacy.cc$petitioner==170 | 
                        legacy.cc$respondent==170),]

pdf("Notes and Slides/NATCasesByTerm25.pdf",6,5)
par(mar=c(4,4,2,2))
hist(NATdata[NATdata$term<1946,]$term,
     xlim=c(1790,1945),breaks=(1945-1790),
     xlab="Term",main=" ")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 1b. The Supreme Court Justices database             ####

Justices<-read.csv("https://epstein.wustl.edu/s/justicesdata2022.csv")

# Example:

pdf("Notes and Slides/SCOTUS-DateNom-AgeNom25.pdf",6,5)
par(mar=c(4,4,2,2))
with(Justices[Justices$recess=="0. no, not recess appointment",],
     plot(yrnom,agenom,main=" ",pch=19,
     ylab="Age at Nomination",xlab="Year of Nomination"))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 1c. "Segal-Cover" and "Martin-Quinn" scores     ####
#
# Segal-Cover:

SC<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/master/Data/Segal-Cover.csv")

# Martin-Quinn:

MQ<-read.csv("http://mqscores.wustl.edu/media/2022/justices.csv")


# Example: Aggregate MQ scores to the justice level 
# (one score per justice for his/her whole career):

MQ.j <- aggregate(post_med~justice,data=MQ,mean)

# Merge with Segal-Cover data:

SC2<-merge(SC,MQ.j,by=c("justice"))

# Plot them against each other:

pdf("Notes and Slides/SCvsMQ25.pdf",6,5)
par(mar=c(4,4,2,2))
with(SC2, plot(Ideology, post_med,pch=20,
               xlab="Segal-Cover (liberalism)",
               ylab="Martin-Quinn (conservatism)",
               xlim=c(-0.3,1.3),ylim=c(-5,3.5))
     )
with(SC2, text(Ideology,post_med,
               labels=SC2$Nominee,pos=1,cex=0.6)
     )
abline(lm(post_med~Ideology,data=SC2),lwd=2)
dev.off()

############################################
# 2. Federal Courts of Appeals...
#
# ...

############################################
# 3. All federal courts...
#
# The Federal Judicial Center's (FJC's)
# biographical database:
#
# https://www.fjc.gov/history/judges

FJC<-read.csv("https://www.fjc.gov/sites/default/files/history/judges.csv")

# Federal judges by Zodiac sign:

FJC$BDate<-paste(FJC$`Birth.Month`,
                 FJC$`Birth.Day`,
                 FJC$`Birth.Year`,
                 sep="-")
FJC$BDate<-mdy(FJC$BDate)
FJC$Sign<-Zodiac(FJC$BDate)

pdf("Notes and Slides/Judge-Zodiac25.pdf",7,5)
par(mar=c(6,4,2,2))
barplot(table(FJC$Sign),las=2,ylab="Frequency")
abline(h=mean(table(FJC$Sign)),lty=2,col="red")
text(1,190,labels="Mean",col="red",cex=0.8)
dev.off()

########################################
# 4. State Supreme Courts...
# ...

########################################
# 5. Inter- and Cross-National Courts...
# ...
