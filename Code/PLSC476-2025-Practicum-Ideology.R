#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This is some code from PLSC 476, dated 
# October 12, 2025. It's a "practicum" day,
# with a focus on judicial ideology.
#
# As always, anything on a line that starts 
# with a "#" is a comment, and will be 
# ignored by the program.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load a (single) necessary R package:

library(plyr)

# Set your working directory:

# setwd("My Files") # <- change this line, and 
                    #    uncomment it to run it

# Other options:

options(scipen = 6) # bias against scientific 
                    # notation
options(digits = 3) # show fewer decimal places
##############################################
# READ IN SOME DATA...
#
# SCDB "votes" data, again:

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

# Read in data on the Martin-Quinn ideology scores:

MQ<-read.csv("http://mqscores.wustl.edu/media/2022/justices.csv")

# ... and pare those data down a bit:

mqthings<-c("term","justice","post_med")
MQ<-MQ[mqthings]
rm(mqthings)
colnames(MQ)<-c("term","justice","MQScore")

# Merge all these things...

DF <- merge(SCDB,SCScores, by=c("justice"),
            all.x=TRUE,all.y=FALSE)
DF <- merge(DF,MQ,by=c("justice","term"),
            all.x=TRUE,all.y=FALSE)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Analysis: Cases involving labor unions...   ####
#
# Subset the data to union-related cases ONLY: 
# Those are the ones where the "issueArea" variable 
# has a value of 7:

UnionDF<-DF[DF$issueArea==7 & is.na(DF$issueArea)==FALSE,]

# Check to make sure that worked:

table(UnionDF$issueArea)

# Q: How many union-related cases has the Court 
# heard? We can plot this by term, but to do that
# we need to aggregate these vote-level data so
# that there is one line of data per case:

CountByTerm<-aggregate(term~docketId,mean,data=UnionDF)
Count<-table(CountByTerm$term)

# Now plot:

pdf("UnionCasesByTerm-25.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Count,xlab="Term",ylab="Number of Union Cases")
dev.off()

# Pro-Union Voting and Decisions...
#
# Note that the database defines pro-union
# votes as "liberal," EXCEPT in cases of 
# union antitrust (where the "issue" variable
# equals 70020). So we can create a pro-
# union vote variable as:

UnionDF$ProUnionVote = UnionDF$direction-1
UnionDF$ProUnionVote<-ifelse(UnionDF$issue==70020,
                             1-UnionDF$ProUnionVote,
                             UnionDF$ProUnionVote)

# Now, examine the fraction of pro-union votes 
# over time. This code creates a yearly summary
# of the fraction of pro-union votes by the
# Court, and a "term" variable to plot it 
# against:

AnnualUnion<-with(UnionDF,prop.table(xtabs(~term+ProUnionVote),1))

# Now graph it:

pdf("AnnualProUnion-25.pdf",6,5)
par(mar=c(4,4,2,2))
plot(rownames(AnnualUnion),AnnualUnion[,2]*100,
     t="o",pch=20,xlab="Term",
     ylab="Pro-Union Vote Percentage")
# draw a dotted line at the overall mean %:
abline(h=mean(UnionDF$ProUnionVote,na.rm=TRUE)*100,
       lty=2) # dotted line for overall average
# add a legend:
legend("topleft",bty="n",lty=2,legend="Overall Average")
dev.off()

# How does this compare to mean Court liberalism scores
# during those years?
#
# Create the mean Segal-Cover score for the sitting 
# justices in each term:

AnnSC <- aggregate(SCScore~term,mean,data=DF)

# Now plot that over time:

pdf("AnnualSC-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(AnnSC, plot(term,SCScore,t="l",lwd=2,
     ylab="Mean Court Liberalism",
     xlab="Term"))
dev.off()

# Same using the (annual) Martin-Quinn measures:

AnnMQ <- aggregate(MQScore~term,mean,data=DF)

# Plot:

pdf("AnnualMQ-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(AnnMQ,plot(term,MQScore,t="l",lwd=2,
                 ylab="Mean Court Conservatism",
                 xlab="Term"))
dev.off()

#++++++++++++++++++++++++++++++++++++++++++++++
# Digression: Pearson's r...               ####  
#
# Generate the figure illustrating Pearson's r 
# that appears in the slides, using simulated
# data...
#
# You wouldn't need to do this for a module
# or anything, but the code is here anyway.

set.seed(7222009)    # set the random number seed
X<-runif(100,0,100)  # generate 100 random observations
                     # from a uniform[0,100] distribution
Y1<-runif(100,0,100) # Y1 is uncorrelated with X
Y2<-((0.01*X)+rnorm(100)+5) # weak positive correlation
Y3<-((-0.1*X)+rnorm(100)+10) # strong negative correlation
r1<-round(cor(X,Y1),2)
r2<-round(cor(X,Y2),2) # calculate Pearson's rs...
r3<-round(cor(X,Y3),2)

# Plot:

pdf("PearsonRs.pdf",8,4)
par(mfrow=c(1,3)) # three side-by-side plots
par(mar=c(4,4,2,2))
plot(X,Y1,pch=19,xlab="X",ylab="Y")
text(20,85,labels=paste0("r = ",r1),col="red")
par(mar=c(4,4,2,2))
plot(X,Y2,pch=19,xlab="X",ylab="Y")
text(20,8,labels=paste0("r = ",r2),col="red")
par(mar=c(4,4,2,2))
plot(X,Y3,pch=19,xlab="X",ylab="Y")
text(80,11,labels=paste0("r = ",r3),col="red")
dev.off()

# End digression...
#++++++++++++++++++++++++++++++++++++++++++++++
#
# Do we observe any aggregate correlation here, 
# by term? Let's plot them and see. Create data:

ProUnion<-data.frame(term=as.numeric(rownames(AnnualUnion)),
                ProUnion=AnnualUnion[,2]*100)
ProUnion<-merge(ProUnion,AnnSC,by=c("term"),all=TRUE)
ProUnion<-merge(ProUnion,AnnMQ,by=c("term"),all=TRUE)

# Calculate correlations:

SC.Cor<-round(with(ProUnion,cor(ProUnion,
                                SCScore,use="complete.obs")),3)
MQ.Cor<-round(with(ProUnion,cor(ProUnion,
                                MQScore,use="complete.obs")),3)

# Plots (side-by-side):

pdf("AnnUnionScatter-25.pdf",8,5)
par(mfrow=c(1,2)) # two side-by-side plots
par(mar=c(4,4,4,2)) # margins
# Segal-Cover plot:
with(ProUnion,plot(SCScore,ProUnion,pch="",xlim=c(0,1),
                   xlab="Segal-Cover Liberalism",
                   ylab="Pro-Union Percentage",
                   main="Segal-Cover"))
with(ProUnion,text(SCScore,ProUnion,labels=term,
                   cex=0.7)) # plot terms
text(0.8,5,labels=paste0("r = ",SC.Cor),col="red") # correlation
# Now the M-Q plot:
par(mar=c(4,4,4,2))
with(ProUnion,plot(MQScore,ProUnion,pch="",xlim=c(-1.5,1),
                   xlab="Martin-Quinn Conservatism",
                   ylab="",
                   main="Martin-Quinn"))
with(ProUnion,text(MQScore,ProUnion,labels=term,
                   cex=0.7)) # plot terms
text(-1,5,labels=paste0("r = ",MQ.Cor),col="red") # correlation
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Justice-level analyses                        ####
#
# Next, we'll examine things at the level of the
# individual justice, by term. We do this because 
# individual justices' ideology -- as measured by
# their Martin-Quinn scores -- can vary over time.
# For example:

pdf("MQChange-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(MQ[MQ$justice==95,],
     plot(term,MQScore,t="l",lwd=2,xlab="Term",
          ylab="Martin-Quinn Score",
          xlim=c(1956,1993),ylim=c(-3.5,2)))
with(MQ[MQ$justice==92,],
     lines(term,MQScore,t="l",lwd=2,lty=2,col="red"))
legend("topleft",bty="n",lwd=2,lty=c(1,2),
       legend=c("Justice White","Justice Brennan"),
       col=c(1,2))
dev.off()

# We can calculate the percentage of votes that 
# each justice cast in a pro-union direction per 
# term as:

ProJT<-aggregate(ProUnionVote~justice+term+justiceName+MQScore,
                 mean,data=UnionDF)
ProJT$ProUnionVote<-ProJT$ProUnionVote*100 # rescale

# What do those aggregated pro-union look like? Well:

pdf("JT-Histogram-25.pdf",6,5)
par(mar=c(4,4,2,2))
hist(ProJT$ProUnionVote,col="grey",main="",
     xlab="Pro-Union Voting Percentage")
dev.off()

# Calculate the Pearson's correlation:

Cor.JT<-round(with(ProJT,cor(ProUnionVote,MQScore,use="complete.obs")),3)

# Plot them against each other:

pdf("JusticeTermScatter-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(ProJT, plot(MQScore,ProUnionVote,pch=20,
                 xlab="Martin-Quinn Conservatism",
                 ylab="Pro-Union Voting Percentage"))
text(-6,10,labels=paste0("r = ",Cor.JT),col="red") # correlation
# add a linear regression line of best fit:
abline(reg=lm(ProUnionVote~MQScore,data=ProJT),lwd=2)
dev.off()

# What does that linear regression look like?

summary(lm(ProUnionVote~MQScore,data=ProJT))

##################################################
# Next, we model individual votes.
#
# Here's the overall vote distribution, for the
# entire data:

pdf("UnionVoteBarplot-25.pdf",6,5)
barplot(prop.table(table(UnionDF$ProUnionVote)),
        ylim=c(0,0.6))
dev.off()

# Please do not ever draw this terrible scatterplot:

pdf("TerribleScatter-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(UnionDF,plot(MQScore,ProUnionVote,pch=19,
                  xlab="Martin-Quinn Score",
                  ylab="Pro-Union Vote Indicator"))
dev.off()

# Let's do something different instead. If justices'
# votes are correlated with their ideology, then 
# there will be a difference in the M-Q scores
# when we split the data into pro- and anti-
# union subsets. Here are boxplots of the M-Q
# scores drawn from pro- vs. anti-union votes:

pdf("MQUnionBoxplots-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(UnionDF,boxplot(MQScore~ProUnionVote,
             xlab="1 = Pro-Union Vote",
             ylab="Martin-Quinn Score (Conservatism)"))
dev.off()

# We can test for a difference statistically, 
# using t-tests:

SC.t<-with(UnionDF,t.test(SCScore~ProUnionVote))
SC.t
MQ.t<-with(UnionDF,t.test(MQScore~ProUnionVote))
MQ.t

# Also, the Pearson correlations between individual votes and
# ideology are:

SCV.cor<-round(with(UnionDF,
               cor(SCScore,ProUnionVote,use="complete.obs")),3)
MQV.cor<-round(with(UnionDF,
               cor(MQScore,ProUnionVote,use="complete.obs")),3)
SCV.cor
MQV.cor

################################################
# Finally: Change over time...
#
# Q: Does the association between ideology and pro-union
# voting remain the same over time, or does it change?
#
# We'll use the vote-level data to get at this. 
# Specifically, we'll calculate the strength of the
# association between ideology and pro-union votes
# separately for each term of the Court, then
# plot those associations over time to see if there 
# is a pattern.
#
# To do this, we'll write a loop function. This
# will 

# First, create a place to put our Pearson 
# correlations between the Martin-Quinn score and
# pro-union voting.

NTerms <- length(unique(UnionDF$term)) # no. of terms 
                                       # in the union data

r.SC<-numeric(NTerms) # These are currently "empty"
r.MQ<-numeric(NTerms) # objects...

# Now make a list of every term in which the Court
# decided at least one union-related case:

TermList <- unique(UnionDF$term) # list of terms in which there
                                 # were union cases decided

# Now, we'll write a loop that will:
#
# 1) Extract the vote-level data from a given Court term,
# 2) Calculate the Pearson's correlation between pro-union
#    votes and the two ideology measures *for that term*,
#    and
# 3) Record each of those correlations in the objects
#    "r.SC" and "r.MQ" that we created above:

for(i in 1:NTerms) { # loop over values from 1 to 72
  # create a single-term dataframe called "df", 
  # with the term equal to the "i"th entry in "TermList":
    df<-UnionDF[UnionDF$term==TermList[i],]
  # Calculate the Pro-union vote / ideology correlation
  # for that term's votes, first using the Segal-Cover
  # measure, then the Martin-Quinn measure, and store
  # that value in the "i"th entry of either r.SC or
  # r.MQ:
    r.SC[i] <- with(df, cor(ProUnionVote,SCScore,
                            use="complete.obs"))
    r.MQ[i] <- with(df, cor(ProUnionVote,MQScore,
                            use="complete.obs"))
} # Done.

# Note that for some of the terms we are not able to
# calculate a Pearson's correlation; that's why you got
# a bunch of warnings...
#
# We can plot the correlation coefficients over time:

pdf("UnionRsOverTime-25.pdf",6,5)
par(mar=c(4,4,2,2))
plot(TermList,r.SC,t="l",lwd=2,xlab="Term",
     ylab="Pearson's Correlation",ylim=c(-1,1),
     xlim=c(1940,2025))
lines(TermList,r.MQ,lwd=2,lty=2,col="red")
abline(h=0,lty=3) # horizontal line at 0
dev.off()

# If we want to know if the strength of the association
# (that is, the Pearson r statistics) are changing
# systematically over time, one way to do that is 
# to treat the Pearson's r coefficients themselves
# as data, and see if they vary systematically with
# the term of the Court. We can use linear regression
# again to do that:

fit.r.SC<-lm(r.SC~TermList)
summary(fit.r.SC)
fit.r.MQ<-lm(r.MQ~TermList)
summary(fit.r.MQ)

# We can illustrate that regression by again plotting
# the Pearson correlations against "term," but this
# time adding a regression line to each:

pdf("RsOverTimeWithLines-25.pdf",8,5)
par(mfrow=c(1,2)) # two columns, one row in the plot
# Segal-Cover first:
par(mar=c(4,4,4,2)) # margins
plot(TermList,r.SC,pch=19,xlab="Term",
     ylab="Pearson's Correlation",
     xlim=c(1940,2025),main="Segal-Cover")
abline(fit.r.SC,lwd=2)
abline(h=0,lty=2) # horizontal line at zero
# Then Martin-Quinn:
par(mar=c(4,4,4,2)) # margins
plot(TermList,r.MQ,pch=19,xlab="Term",
     ylab="Pearson's Correlation",
     xlim=c(1940,2025),main="Martin-Quinn")
abline(fit.r.MQ,lwd=2)
abline(h=0,lty=2) # horizontal line at zero
dev.off()

# See the slides for conclusions, etc.
