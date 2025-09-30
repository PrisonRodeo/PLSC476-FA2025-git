#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This is some code from PLSC 476,
# dated September 30, 2025. The topic
# is political ideology, and its
# relationship to judicial behavior
# (here, on the U.S. Supreme Court).
#
# Note that anything on a line
# that starts with a "#" is a 
# comment, and will be ignored by
# the program (but not by you).
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load some R packages:               ####

# install.packages("plyr") # <-- perhaps uncomment

library(plyr)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Read in SCOTUS votes + Segal-Cover ideology data: ####

unlink("SCDBData/*.*", recursive = TRUE)

url<-"https://scdb.la.psu.edu/?jet_download=f63edb5d812a973b488eb48e42935b673c8987b3"
path<-"SCDBData/modjustice.zip"  # Local file name

download.file(url,path,mode="wb")
unzip("SCDBData/modjustice.zip",exdir="SCDBData")
filename<-dir("SCDBData")[2]

Votes<-read.csv(paste0("SCDBData/",filename))
rm(filename,path,url)

# Segal-Cover scores data:

SCScores <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/master/Data/Segal-Cover.csv") # read the Segal-Cover data

# Fix Rehnquist (remove his CJ line of data...)

SCScores<-SCScores[SCScores$Order!=32, ]

# Merge:

Master <- merge(Votes,SCScores,by=c("justice"),
                all.x=TRUE,all.y=FALSE)

# Plot ideology vs. qualifications:

pdf("SCScores-25.pdf",7,6)
par(mar=c(4,4,2,2))
seed<-7222009 # random number seed, for jittering
with(SCScores, plot(Qualifications,Ideology,pch=20,
                    ylab="Ideology",xlab="Qualifications",
                    xlim=c(-0.1,1.2),ylim=c(0,1.1)))
with(SCScores, text(Qualifications,jitter(Ideology,10),
                    labels=Nominee,pos=3,cex=0.8))
text(0,1.1,labels=paste0("r = ",round(cor(SCScores$Ideology,
                              SCScores$Qualifications,
                              use="complete.obs"),2)),cex=1.5)
dev.off()

# Ideology & voting, all cases...
# 
# First, aggregate the votes by justice, keeping
# the justice's name and Segal-Cover scores:

Master$LibVote <- Master$direction-1 # Liberal vote indicator
DF<-aggregate(LibVote~justice+justiceName+Ideology,
              data=Master,mean)
DF$LibVotePct <- DF$LibVote*100

# Now, plot (plus OLS regression line):

pdf("SCvsVotes-25.pdf",7,6)
par(mar=c(4,4,2,2))
with(DF, plot(Ideology,LibVotePct,pch=20,
              ylab="Liberal Voting Percent",xlab="Ideology",
              xlim=c(-0.1,1.1),ylim=c(25,80)))
with(DF, text(Ideology,LibVotePct,Ideology,
              labels=justiceName,pos=3,cex=0.8))
with(DF, abline(lm(LibVotePct~Ideology),lwd=3,col="red"))
dev.off()

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Segal et al. (1995):                         ####
#
# "Replicate"  (in part) Segal et al. (1995)
#
# Create data for Table 2:

CRCL<-Master[Master$issueArea<6,] # civ. rts. & libs
EC<-Master[Master$issueArea==8,]  # economics cases

Civ<-ddply(CRCL, .(justice), summarize,
           LibVote = mean(LibVote,na.rm=TRUE)*100)
Civ$President<-NULL # remove before merging
Civ<-merge(Civ,SCScores,by="justice")

Econ<-ddply(EC, .(justice), summarize,
            LibVote = mean(LibVote,na.rm=TRUE)*100)
Econ$President<-NULL # remove before merging
Econ<-merge(Econ,SCScores,by="justice")

# Plot Civil rights / liberties:

pdf("SCvsCRCL-25.pdf",4,6)
par(mar=c(4,4,4,2))
with(Civ, plot(Ideology,LibVote,pch=20,
               ylab="Liberal Voting Percent",xlab="Ideology",
               xlim=c(-0.2,1.2),ylim=c(10,100),
               main="Civil Rights and Liberties"))
with(Civ, text(Ideology,LibVote,Ideology,
               labels=Nominee,pos=3,cex=0.6))
with(Civ, abline(lm(LibVote~Ideology),lwd=3,col="red"))
dev.off()

# Plot economics:

pdf("SCvsEcon-25.pdf",4,6)
par(mar=c(4,4,4,2))
with(Econ, plot(Ideology,LibVote,pch=20,
               ylab="Liberal Voting Percent",xlab="Ideology",
               xlim=c(-0.2,1.2),ylim=c(10,100),
               main="Economics"))
with(Econ, text(Ideology,LibVote,Ideology,
               labels=Nominee,pos=3,cex=0.6))
with(Econ, abline(lm(LibVote~Ideology),lwd=3,col="red"))
dev.off()

# Table 3 correlations, updated:

CivR1<-with(Civ[Civ$Year<1952,], 
            cor(Ideology,LibVote,use="complete.obs")) # FDR & Truman

CivR2<-with(Civ[Civ$Year>1952,], 
            cor(Ideology,LibVote,use="complete.obs")) # Eisenhower-Trump

CivR3<-with(Civ,cor(Ideology,LibVote,use="complete.obs")) # All

EconR1<-with(Econ[Civ$Year<1952,], 
             cor(Ideology,LibVote,use="complete.obs")) # FDR & Truman

EconR2<-with(Econ[Civ$Year>1952,],
             cor(Ideology,LibVote,use="complete.obs")) # Eisenhower-Obama

EconR3<-with(Econ, cor(Ideology,LibVote,use="complete.obs")) # All

round(c(CivR1,EconR1,CivR2,EconR2,CivR3,EconR3),2)

# Now, "Actual" vs. "Predicted". When we fit a regression
# line to some data, the difference between the line 
# and the actual point is called a "residual." The
# residual shows whether the justice in question voted
# more or less liberally than we would have expected
# them to based on their Segal-Cover score.
#
# We can do the same plots Segal et al. did, for the 
# post-Truman justices. 
# 
# First, subset the data to post-Truman appointees only:

CivNew<-Civ[Civ$Year>1952,]

EconNew<-Econ[Econ$Year>1952,]

# Now fit a regression line to the civil rights /
# liberties data, and generate predictions:

CFit<-with(CivNew, lm(LibVote~Ideology))
CPreds<-CFit$fitted.values

# Now plot one vs. the other:

pdf("CivLibsAvP-25.pdf",7,6)
plot(CPreds,CFit$model$LibVote,pch=20,
     ylab="Actual Support", 
     xlab="Predicted Support",
     xlim=c(10,100),ylim=c(10,100),
     main="Civil Rights and Liberties")
abline(a=0,b=1,lwd=2,col="red")
dev.off()

# Now repeat for economics cases...

EFit<-with(EconNew, lm(LibVote~Ideology))
EPreds<-EFit$fitted.values

# Now plot one vs. the other:

pdf("EconAvP-25.pdf",7,6)
plot(EPreds,EFit$model$LibVote,pch=20,
     ylab="Actual Support", 
     xlab="Predicted Support",
     xlim=c(10,100),ylim=c(10,100),
     main="Economics")
abline(a=0,b=1,lwd=2,col="red")
dev.off()

# Finally: Ginsburg and Breyer, vs. the Segal
# et al. predictions:

RBG.Civ<-with(Civ[Civ$Nominee=="Ruth Bader Ginsburg",],LibVote)
RBG.Econ<-with(Econ[Econ$Nominee=="Ruth Bader Ginsburg",],LibVote)
SGB.Civ<-with(Civ[Civ$Nominee=="Stephen G. Breyer",],LibVote)
SGB.Econ<-with(Econ[Econ$Nominee=="Stephen G. Breyer",],LibVote)

c(RBG.Civ,RBG.Econ,SGB.Civ,SGB.Econ) # display

# Barrett predictions...

ACB <- data.frame(Ideology=0.23) # Barrett's Segal-Cover score

CL.ACB<-predict(CFit,ACB)   # Predict CL voting
Econ.ACB<-predict(EFit,ACB) # Predict Econ voting

c(CL.ACB,Econ.ACB) # Display predictions