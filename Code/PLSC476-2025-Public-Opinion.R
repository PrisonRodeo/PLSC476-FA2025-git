#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This is some code from PLSC 476,
# dated October 14, 2025.
#
# The subject matter is judicial behavior
# and public opinion.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load necessary R packages...            ####
#
# By now, you should already have installed the R 
# packages "readr", "plyr", "dplyr", psych", 
# and "stargazer". If you have not, uncomment 
# and run this code:
#
# install.pcakages("readr")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("psych")
# install.packages("stargazer")
#
# Then do:

library(readr)
library(plyr)
library(dplyr)
library(psych)
library(stargazer)

# Set a couple options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# And be sure to use -setwd- to set the local / 
# working directory to where you need it to be.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Read in SCOTUS votes + Segal-Cover ideology data: ####

unlink("SCDBData/*.*", recursive = TRUE)

url<-"https://scdb.la.psu.edu/?jet_download=f63edb5d812a973b488eb48e42935b673c8987b3"
path<-"SCDBData/modjustice.zip"  # Local file name

download.file(url,path,mode="wb")
unzip("SCDBData/modjustice.zip",exdir="SCDBData")
filename<-dir("SCDBData")[2]

SCDB<-read.csv(paste0("SCDBData/",filename))
rm(filename,path,url)

# Segal-Cover scores data:

SCScores <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/master/Data/Segal-Cover.csv") # read the Segal-Cover data

# Fix Rehnquist (remove his CJ line of data...)

SCScores<-SCScores[SCScores$Order!=32, ]

# ...and pare the data down to just what we need:

scthings<-c("justice","Ideology","Qualifications")
SCScores<-SCScores[scthings]
rm(scthings)
colnames(SCScores)<-c("justice","Ideology","SCQual")

# merge:

Master <- merge(SCDB,SCScores, by=c("justice"),
                all.x=TRUE,all.y=FALSE)

# Read in annual "public mood" data:

Mood <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/refs/heads/main/Data/PublicMood25.csv")

# Create a {0,1} liberal vote variable:

Master$LibVote <- Master$direction - 1

# Now, as in McGuire & Stimson, we focus only on 
# Supreme Court *reversals*...
#
# Drop cases where SCOTUS affirmed the lower court:

Master$Rev <- ifelse(Master$caseDisposition>1 & 
                       Master$caseDisposition<5,1,0)
Reversals <- Master[Master$Rev==1,]

# Create three files for Criminal, CivLibs, and Economics:

CrimRev <- Reversals[Reversals$issueArea==1,]
CivlibRev <- Reversals[Reversals$issueArea==2,]
EconRev <- Reversals[Reversals$issueArea==8,]

# Aggregate the votes up to the term level...

CrimAnn <- ddply(CrimRev, "term", summarize,
                 SegalCover = mean(Ideology,na.rm=TRUE)*100,
                 CrimLibPct = (mean(LibVote,na.rm=TRUE))*100)
CLAnn <- ddply(CivlibRev, "term", summarize,
                 CLLibPct = (mean(LibVote,na.rm=TRUE))*100)
EconAnn <- ddply(EconRev, "term", summarize,
                 EconLibPct = (mean(LibVote,na.rm=TRUE))*100)

# Build annual "time series" dataset:

colnames(Mood) <- c("term","Mood")
TSData <- merge(Mood,CrimAnn,by=c("term"))
TSData <- merge(TSData,CLAnn,by=c("term"))
TSData <- merge(TSData,EconAnn,by=c("term"))

# Examine summary statistics, plots, and correlations:

describe(TSData)

pdf("MoodPlot25.pdf",7,6)
par(mar=c(4,4,2,2))
with(TSData, plot(term,Mood,t="l",lwd=3,col="black",ylim=c(15,85),
                  xlim=c(1948,2025),xlab="Term",ylab="Liberalism"))
with(TSData, lines(term,CrimLibPct,lwd=2,col="blue"))
with(TSData, lines(term,CLLibPct,lwd=2,col="red"))
with(TSData, lines(term,EconLibPct,lwd=2,col="orange"))
legend("bottomleft",c("Mood","Criminal","Civil Rights","Economics"),
       lwd=c(3,2,2,2),col=c("black","blue","red","orange"),
       bty="n",cex=0.85)
dev.off()

# Correlations & scatterplots:

cors<-cor(TSData)

cors

pdf("MoodScatters25.pdf",7,3)
par(mar=c(4,4,2,2))
par(mfrow=c(1,3))
with(TSData, plot(Mood,CrimLibPct,pch=20,col="blue",
                  ylab="Criminal Liberalism",
                  xlim=c(50,75)))
text(55,70,labels=paste0("r = ",round(cors[4,2],2)))
with(TSData, plot(Mood,CLLibPct,pch=20,col="red",
                  ylab="Civil Rights Liberalism",
                  xlim=c(50,75)))
text(55,80,labels=paste0("r = ",round(cors[5,2],2)))
with(TSData, plot(Mood,EconLibPct,pch=20,col="orange",
                  ylab="Economic Liberalism",
                  xlim=c(50,75)))
text(55,77,labels=paste0("r = ",round(cors[6,2],2)))
dev.off()

# BUT: Need to control for Court ideology and a
# lagged "dependent variable"...
#
# Three linear regression analyses:

CrimFit<-with(TSData, lm(CrimLibPct~lag(CrimLibPct)+SegalCover+Mood))
CLFit<-with(TSData, lm(CLLibPct~lag(CLLibPct)+SegalCover+Mood))
EconFit<-with(TSData, lm(EconLibPct~lag(EconLibPct)+SegalCover+Mood))

# Now put those in a pretty table:

stargazer(CrimFit,CLFit,EconFit,type="text",
          column.labels=c("Criminal","Civil Liberties",
                          "Economics"),
          dep.var.labels=c("","",""))

# The LaTeX version:
# 
stargazer(CrimFit,CLFit,EconFit,type="latex",
          column.labels=c("Criminal","Civil Liberties",
                          "Economics"),
          dep.var.labels=c("","",""))
