#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This is some code from PLSC 476, dated
# October 9, 2025
#
# The code is for a practicum on "legal" and
# "integrated" models of judicial behavior. It 
# also introduces multivariate linear regression,
# including the linear probability model.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preliminaries                                   ####
#
# Load a few necessary R packages [if you haven't
# installed this one by now, you should do so, 
# using -install.packages()-]:

library(plyr)
library(psych)

# Set a couple options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also be sure to use -setwd()- to set your
# working directory (that is, where any files
# you create will be kept).
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Read in SCOTUS votes + Segal-Cover data...      ####

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

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Analysis: Cases involving habeas corpus     ####
#
# Subset habeas cases ONLY: Those are the ones
# where the "issue" variable has values of 10020.

Habeas<-Master[Master$issue==10020,]

# Also, make a case-level data frame, for some
# summary statistics:

HCases<-ddply(Habeas, .(docketId,term,lawSupp),summarize,
              NMaj=sum(majority-1),
              ProHabeas = mean(decisionDirection-1))

# Plot number of cases per term:

TermData<-ddply(HCases,.(term),summarize,
                NCases=length(term),
                ProPct=(mean(ProHabeas)*100))

pdf("HabeasByTerm-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(TermData, plot(term,NCases,t="l",lwd=3,xlab="Term",
                    ylab="Number of Cases",))
abline(v=1996,lwd=2,lty=2,col="navy")
text(1996,1.6,"AEDPA",pos=4,col="navy")
dev.off()

# How many pro- and anti-claimant outcomes?:

pdf("HabeasOutcomes-25.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(table(HCases$ProHabeas),col="navy",
        names.arg=c("Anti-Habeas","Pro-Habeas"))
dev.off()

# How do pro- and anti-claimant outcomes vary over
# time?

pdf("HabeasOutcomesByTerm-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(TermData, barplot(ProPct,xlab="Term",
                    names.arg=term,col="navy",
                    ylab="Percent Pro-Habeas",))
dev.off()

# Table of case-level laws:

table(HCases$lawSupp)

# Now, create variables to use in the analysis.
# The outcome is each justice's vote, either 
# pro-habeas ("liberal") or anti-habeas 
# ("conservative"):

Habeas$ProVote<-Habeas$direction-1

# Make the variable "Ideology" the Segal-
# Cover score for each justice, coded with "1"
# as most liberal and "0" as most conservative.

Habeas$Ideology<-Habeas$SCScore

# Is the defendant the petitioner? Code this 1
# if yes, 0 otherwise
#
# Petitioner is a prisoner:

Habeas$CrimPet<-ifelse(Habeas$petitioner==126,1,0)
Habeas$CrimPet<-ifelse(Habeas$petitioner==215,1,Habeas$CrimPet)

# Petitioner is a defendant/accused of a crime:

Habeas$CrimPet<-ifelse(Habeas$petitioner==100,1,Habeas$CrimPet)

# Was the legal matter in the case applicability
# of federal habeas, or something else?

Habeas$HabLaw<-ifelse(Habeas$lawSupp==341,1,0)

# Was there disagreement in the lower courts? That is
# the "lcDisagreement" variable; it equals "1" if there
# was such disagreement and 0 otherwise.

Habeas$Disagree<-Habeas$lcDisagreement

# Last: An indicator (=1) for cases heard after
# passage of the AEDPA in 1996:

Habeas$AEDPA<-ifelse(Habeas$term>1996,1,0)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Summary statistics

Vars<-with(Habeas, data.frame(ProVote,Ideology,CrimPet,
                              HabLaw,Disagree,AEDPA))
summary(Vars)
describe(Vars)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Some bivariate statistics, looking at the
# correlation between each of those variables
# and the votes of the justices:

options(digits=2)
cor(Vars,use="complete.obs")

# Now, a multivariate / regression analysis:

Regression <- with(Habeas,
                   lm(ProVote~Ideology+CrimPet+HabLaw+
                        Disagree+AEDPA))
summary(Regression)

# This suggests that:
# 
# - More liberal justices are substantially more likely to 
# vote in a pro-habeas direction than are conservatives.
#
# - Justices are also more likely to vote in a pro-habeas 
# direction in cases where the detainee is bringing the 
# petition (that is, when a pro-habeas vote is a vote to 
# reverse the lower court).
#
# - Justices are also (all else equal) more likely to vote 
# in a pro-habeas direction in cases decided after the 
# passage of the AEDPA in 1996.
#
# - Justices are somewhat less likely to vote in a 
# pro-habeas direction when the case involves a question 
# of the applicability of federal habeas law.
#
# - We find no conditional association between pro-habeas 
# votes and lower court disagreement.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Lastly: Suppose you want to take your cool 
# regression results and show them via a dotplot, 
# or put them in a nice-looking table which you 
# can then just import into (say) Microsoft Word...
#
# First, the dotplot:

# install.packages("dotwhisker") # <-- uncomment this
library(dotwhisker)              #     the first time you
                                 #     run this code.
# 
# This turns a "lm object" (that is, the results
# of a linear regression) into a plot. Here's the
# simple version:

dwplot(Regression)

# This packages uses the command structure /
# terminology of the --ggplot-- suite of 
# commands. Here's a prettier (but also more
# complex) version of the same plot:

pdf("HabeasRegressionPlot-25.pdf",6,4)
dwplot(Regression,show_intercept=TRUE) |>
  relabel_predictors(c(
      Ideology = "Justice Liberalism",
      CrimPet = "Prisoner Petition",
      HabLaw = "Federal Statute",
      Disagree = "Lower Ct. Disagreement",
      AEDPA = "Post-AEDPA")) +
  theme_bw() + xlab("Estimated Coefficient") + ylab("") +
  geom_vline(xintercept=0,colour="black",linetype=2) +
  theme(legend.position="none")
dev.off()

# Next, we'll do the MS-Word table...
#
# install.packages("flextable") # <-- uncomment these
library(flextable)              #     the first time you
# install.packages("officer")   # <-- run this code.
library(officer)

# "flextable" turns R output into a format 
# that is amenable to bringing into MS-Word
# (or other formatted) documents. "officer"
# (think "Office R") lets you tweak Word
# documents in R.
#
# More information is available here:
#
# https://davidgohel.github.io/flextable/index.html
#
# and here:
#
# https://davidgohel.github.io/officer/
#
# Here's how we can turn those regression
# results into a table for Word:

WordTable<-as_flextable(Regression)

# That creates a Word-ready (flex)table. Next, 
# we can create a MS-Word object in R, and put the
# table in it:

WordObject<-read_docx()
WordObject<-body_add_par(WordObject,"My Regression Table",
                      style = "heading 2")
WordObject<-body_add_flextable(WordObject,value=WordTable)

# Finally, we can write that to a Word document on
# your local drive (or wherever):

MyWordDoc<-print(WordObject,target="MyRegressionTable-25.docx")

# Now "MyRegressionTable.docx" can be opened with
# M$-Word, edited, cut-and-pasted, etc.
