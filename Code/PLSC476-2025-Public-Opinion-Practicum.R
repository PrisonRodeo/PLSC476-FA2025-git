#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This is some code from PLSC 476,
# dated October 16, 2025.
#
# The subject matter is (again) judicial 
# behavior and public opinion.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load necessary R packages...            ####

P<-c("readr","plyr","dplyr","ggplot2","psych",
     "readxl","stargazer","car")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# ^ run that block 5-6 times until you get all smiley faces :)
#
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

# Create a {0,1} liberal vote variable:

Master$LibVote <- Master$direction - 1

# Next: Public opinion data. Read in annual "public 
# mood" data:

Mood <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/refs/heads/main/Data/PublicMood25.csv")

#... and also the "abortion mood" data:

AbMood<-read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/refs/heads/main/Data/AbortionMood.csv")

# Next, we have to merge the Mood and Abortion Mood data with the 
# SCDB. To do that, we take advantage of the fact that the mood variables
# are years, while the *term* variable is the October Term of SCOTUS.
#
# So:

Master$Year<-Master$term

# Merge:

Master<-merge(Master,Mood, by=c("Year"),all=TRUE)
Master<-merge(Master,AbMood, by=c("Year"),all=TRUE)

#=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Part I: Mood and SCOTUS Votes
#
# Is there an association between the justices' votes and
# the (omnibus) "public mood" measure?
#
# We're going to subset the data to eliminate cases/votes
# where we don't have Mood data (basically, decisions before
# 1952), and cases where Liberal Vote is missing:

DF<-Master[is.na(Master$Mood)==FALSE,]
DF<-DF[is.na(DF$LibVote)==FALSE,]
DF$VoteLabel<-ifelse(DF$LibVote==1,paste("Liberal"),paste("Conservative"))
DF$Reversal<-ifelse(DF$partyWinning==1,1,0)
DF$Ideology<-DF$Ideology*100 # rescale, for comparability

# Summary statistics on some interesting variables:

v<-c("Mood","AbMood","LibVote","Ideology","Reversal")
describe(DF[v])

# Here's a plot:

cols<-c("blue","orange")

pdf("MoodVote25.pdf",8,6)
par(mar=c(4,4,2,2))
ggplot(DF, aes(x=Mood, fill=factor(VoteLabel))) +
  geom_density(alpha = 0.6,bw=1,na.rm=TRUE) +
  labs(fill="Vote Direction",x="Mood",y="Density") +
  scale_fill_manual(values = cols) + 
  theme_classic()
dev.off()

# Is there a (bivariate) statistical difference?

with(DF, t.test(Mood~LibVote))

# Is justice ideology related to mood / public opinion?

pdf("IdeoMood25.pdf",7,6)
par(mar=c(4,4,2,2))
scatterplot(Ideology~Mood,data=DF,pch=19,col="black")
dev.off()

# Regression...

fitOLS<-lm(LibVote~Ideology+Mood,data=DF)
summary(fitOLS)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Reversals vs. affirmances                       ####
#
# In general, affirmances are less "ideological"
# than reversals. We can see this by aggregating to
# the level of the justice. First, all cases:

AllJ<-aggregate(LibVote~justice+justiceName+Ideology,
                 mean,data=DF)

# ...then reversals and affirmances separately:

AffJ<-aggregate(LibVote~justice+justiceName+Ideology,
                mean,data=DF[DF$Reversal==0,])

RevJ<-aggregate(LibVote~justice+justiceName+Ideology,
                mean,data=DF[DF$Reversal==1,])

# Now plot all three:

allr<-with(AllJ, cor(LibVote,Ideology)) # correlations
affr<-with(AffJ, cor(LibVote,Ideology))
revr<-with(RevJ, cor(LibVote,Ideology))

pdf("AllAffRevScatters25.pdf",7,3)
par(mar=c(4,4,2,2))
par(mfrow=c(1,3))
with(AllJ, plot(Ideology,LibVote*100,pch=20,col="black",
                  ylab="Liberal Vote Percentage",
                  xlab="Justice Liberalism",
                  ylim=c(20,85),main="All Cases"))
text(20,80,labels=paste0("r = ",round(allr,2)))
with(RevJ, plot(Ideology,LibVote*100,pch=20,col="blue",
                ylab="Liberal Vote Percentage",
                xlab="Justice Liberalism",
                ylim=c(20,85),main="Reversals"))
text(20,80,labels=paste0("r = ",round(revr,2)))
with(AffJ, plot(Ideology,LibVote*100,pch=20,col="darkorange",
                ylab="Liberal Vote Percentage",
                xlab="Justice Liberalism",
                ylim=c(20,85),main="Affirmances"))
text(20,80,labels=paste0("r = ",round(affr,2)))
dev.off()

# This suggests that we might want to separate the 
# affirmances and reversals, to see if there are
# different dynamics around mood there as well.
# We can run two more regressions on separate data 
# frames, and put them in a table to see:

fitRev<-lm(LibVote~Ideology+Mood,data=DF[DF$Reversal==1,]) # reversals
fitAff<-lm(LibVote~Ideology+Mood,data=DF[DF$Reversal==0,]) # affirmances

# Now put those in a pretty table:

stargazer(fitOLS,fitRev,fitAff,type="text",
          column.labels=c("All Cases","Reversals",
                          "Affirmances"),
          dep.var.labels=c("","",""))

# The LaTeX version:
# 
# stargazer(fitOLS,fitRev,fitAff,type="latex",
#           column.labels=c("All Cases","Reversals",
#                           "Affirmances"),
#           dep.var.labels=c("","",""))
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Public Opinion and Abortion Decisions       ####
#
# First, let's subset and summarize the data:

AbDF<-DF[DF$issue==50020,]

describe(AbDF[v])

# At the justice-aggregated level, is there any 
# evidence of an association between public 
# opinion on abortion and the justices' votes
# in abortion rights cases?
#
# Aggregate the abortion data to the annual
# level:

AbAnnual<-aggregate(LibVote~Year+AbMood, # aggregate
                mean,data=AbDF)

AbAnnual<-AbAnnual[order(AbAnnual$Year),] # sort by year

Abr<-with(AbAnnual, cor(LibVote,AbMood)) # correlation

pdf("AbTSPlot25.pdf",7,6)
par(mar=c(4,4,2,2))
with(AbAnnual, plot(Year,LibVote*100,t="l",lwd=3,
                  col="black",ylim=c(0,100),
                  xlim=c(1969,2025),xlab="Term",ylab="Score"))
with(AbAnnual, lines(Year,AbMood,lwd=2,col="orange"))
text(2010,90,paste0("r = ",round(Abr,2)))
legend("topleft",c("Abortion Votes","Abortion Mood"),
       lwd=c(3,2,2,2),col=c("black","orange"),
       bty="n",cex=0.85)
dev.off()

# Now, with the disaggregated / vote-level data, we
# can considerthe regression of voting on ideology 
# and mood in abortion cases. Once again, we'll do 
# that for all cases, and then for reversals and 
# affirmances separately:

AbFit<-lm(LibVote~Ideology+Mood,data=AbDF) # all
AbFitRev<-lm(LibVote~Ideology+Mood,data=AbDF[AbDF$Reversal==1,]) # reversals
AbFitAff<-lm(LibVote~Ideology+Mood,data=AbDF[AbDF$Reversal==0,]) # affirmances

# Now put those in a pretty table:

stargazer(AbFit,AbFitRev,AbFitAff,type="text",
          column.labels=c("All Cases","Reversals",
                          "Affirmances"),
          dep.var.labels=c("","",""))

# The LaTeX version:
# 
stargazer(AbFit,AbFitRev,AbFitAff,type="latex",
          column.labels=c("All Cases","Reversals",
                          "Affirmances"),
          dep.var.labels=c("","",""))

# /fin