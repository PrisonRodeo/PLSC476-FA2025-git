#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This is some code from PLSC 476,
# dated October 7, 2025. The topic
# is "legal influences" on judicial
# behavior. Once again, the focus is
# on the U.S. Supreme Court. This 
# code is pretty much entirely focused
# on the topic of precedent and
# stare decisis.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries...                                ####
#
# Load some R packages:

# install.packages("RCurl") # <-- perhaps uncomment

library(plyr)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# Set a working directory in here somewhere too...
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Prepare data...                                 ####
#
# Read in SCOTUS votes + Segal-Cover ideology data:

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
colnames(SCScores)<-c("justice","SCScore","SCQual")

# merge:

Master <- merge(SCDB,SCScores, by=c("justice"),
            all.x=TRUE,all.y=FALSE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Analysis: Cases alteration of precedent...            ####
#
# See the slides for details. We're examining two different
# phenomena: Sizes of majorities, and ideological voting
#
# First, how many cases are there in which precedent
# was overturned?

Cases <- ddply(Master, .(docketId), summarize,
               AltPrec = mean(precedentAlteration,na.rm=TRUE),
               MajVotes = mean(majVotes,na.rm=TRUE),
               Term = mean(term))
Ns <- table(Cases$AltPrec)

pdf("PrecAlterNs-25.pdf",6,5)
par(mar=c(4,4,3,2))
foo<-with(Cases, barplot(table(AltPrec),col="navy",ylim=c(0,12000),
                 names.arg=c("No Alteration","Alteration"),
                 ylab="Number of Cases"))
text(foo,table(Cases$AltPrec),
     paste0(round(prop.table(Ns),4)*100,c(" percent"," percent")),
     pos=3)
dev.off()

# During this period, when did those alterations
# happen?

Terms<-aggregate(AltPrec~Term,data=Cases,sum)

pdf("AltPrecByTerm-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(Terms, plot(Term,AltPrec,t="l",lwd=2,xlab="Term",
                 ylab="Number of Alterations of Precedent",
                 xlim=c(1940,2020)))
dev.off()

# Make a liberal vote variable:

Master$LibVote <- Master$direction-1

# Separate the "Master" and "Case" data into two piles:
# those where precedent is altered and those where
# it is not:

AltV <- Master[Master$precedentAlteration==1,] # votes, alter prec.
NoAltV <- Master[Master$precedentAlteration==0,] # votes, no alter

AltC <- Cases[Cases$AltPrec==1,] # cases, alter precedent
NoAltC <- Cases[Cases$AltPrec==0,] # cases, no alteration

# Examine sizes of majority coalitions in precedent-
# altering vs. precedent non-altering decisions, using 
# the case-level data:

with(AltC, mean(MajVotes,na.rm=TRUE))
with(NoAltC, mean(MajVotes,na.rm=TRUE))

with(Cases, t.test(MajVotes~AltPrec))

# Non-precedent-altering cases have slightly but
# significantly larger majorities than those that alter
# existing precedent.
#
# Next: examine the correlation between Segal-Cover scores 
# and vote direction (liberal or conservative) in the two
# data sets:

with(AltV, cor(SCScore,LibVote,use="complete.obs"))
with(NoAltV, cor(SCScore,LibVote,use="complete.obs"))

# Correlations between ideology and voting are higher
# in cases involving alteration of precedent.