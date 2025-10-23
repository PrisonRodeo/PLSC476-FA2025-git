#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# This is some code from PLSC 476,
# dated October 23, 2025.
#
# The subject matter is empirical models of
# the separation of powers.

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load necessary R packages...                    ####

P<-c("readr","plyr","ggplot2","psych",
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
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Vote- and Decision-Level data...                ####
#
# Read in SCOTUS votes + decisions + Segal-Cover 
# ideology data:

unlink("SCDBData/*.*", recursive = TRUE)

# Votes:

url<-"https://scdb.la.psu.edu/?jet_download=f63edb5d812a973b488eb48e42935b673c8987b3"
path<-"SCDBData/modjustice.zip"  # Local file name

download.file(url,path,mode="wb")
unzip("SCDBData/modjustice.zip",exdir="SCDBData")
filename<-dir("SCDBData")[2]

Votes<-read.csv(paste0("SCDBData/",filename))
rm(filename,path,url)

# Cases:

url<-"https://scdb.la.psu.edu/?jet_download=c29d10b22077fe9fbb5a80a624a4ab1bd50ab536"
path<-"SCDBData/modcases.zip"  # Local file name

download.file(url,path,mode="wb")
unzip("SCDBData/modcases.zip",exdir="SCDBData")
filename<-dir("SCDBData")[3]

Cases<-read.csv(paste0("SCDBData/",filename))
rm(filename,path,url)

# Segal-Cover scores data:

SCScores <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/master/Data/Segal-Cover.csv") # read the Segal-Cover data

# Fix Rehnquist (remove his CJ line of data...)

SCScores<-SCScores[SCScores$Order!=32, ]

# Rename:

SCScores$JusticeLiberalism<-SCScores$Ideology # rename the Segal-Cover
SCScores$Ideology<-NULL                       # variable

# Merge:

MasterV <- merge(Votes,SCScores,by=c("justice"),
                all.x=TRUE,all.y=FALSE)
MasterV <- plyr::rename(MasterV,c("President"="PresidentName"))

# Aggregate vote-level Segal-Cover data to the case 
# level...

SCCase<-aggregate(JusticeLiberalism~docketId,data=MasterV,mean)

# Merge this with the case-level data:

MasterC <- merge(Cases,SCCase,by=c("docketId"),
                 all.x=TRUE,all.y=FALSE)

# Now pull the NOMINATE (Congressional and presidential
# ideology) data:

NOM<-read_csv("https://voteview.com/static/data/out/members/HSall_members.csv")

# Aggregate to the level of the Congress, keeping MEDIAN
# first-dimension NOMINATE scores:

NOM.c<-aggregate(nominate_dim1~chamber+congress,data=NOM,median)

# Pull apart House, Senate, & President, and rename
# the NOMINATE / ideology variables:

PRES<-NOM.c[NOM.c$chamber=="President",]
PRES<-rename(PRES,c("nominate_dim1" = "President"))
PRES$chamber<-NULL
HOUSE<-NOM.c[NOM.c$chamber=="House",]
HOUSE<-rename(HOUSE,c("nominate_dim1" = "House"))
HOUSE$chamber<-NULL
SEN<-NOM.c[NOM.c$chamber=="Senate",]
SEN<-rename(SEN,c("nominate_dim1" = "Senate"))
SEN$chamber<-NULL

# Merge back together by Congress:

NOM.cong<-merge(HOUSE,SEN,by=c("congress"),all=TRUE)
NOM.cong<-merge(NOM.cong,PRES,by=c("congress"),all=TRUE)
rm(NOM,NOM.c,PRES,HOUSE,SEN) # clean up

# Add years variables that maps to Congresses:

NOM.cong$YEAR <- 1787 + (2*NOM.cong$congress)

# Duplicate, change the year, and combine, to yield
# annual data:

NOM.dup<-NOM.cong
NOM.dup$YEAR<-NOM.dup$YEAR+1
NOM.ann<-data.frame(rbind(NOM.cong,NOM.dup))
NOM.ann<-NOM.ann[order(NOM.ann$YEAR),]  # sort by year
rm(NOM.cong,NOM.dup) # clean up

# Plot these over time, to see what they look like:

pdf("AllNOMINATE-25.pdf",6,5)
par(mar=c(4,4,2,2))
with(NOM.ann,plot(YEAR,President,t="l",lwd=2,
                  xlim=c(1788,2023),ylim=c(-0.75,0.75),
                  xlab="Year",ylab="Ideology (Conservatism)"))
with(NOM.ann,lines(YEAR,House,t="l",lwd=2,lty=2,col="darkblue"))
with(NOM.ann,lines(YEAR,Senate,t="l",lwd=2,lty=3,col="orange"))
legend("topleft",bty="n",lwd=2,lty=c(1,2,3),
       col=c("black","darkblue","orange"),legend=c("President",
                                                   "House","Senate"))
dev.off()

# Now, merge the annual NOMINATE data with the case- and
# vote-level Supreme Court data:

NOM.ann$term<-NOM.ann$YEAR # create a term variable

MasterC<-merge(MasterC,NOM.ann, by=c("term"),
               all.x=TRUE,all.y=FALSE)

MasterV<-merge(MasterV,NOM.ann, by=c("term"),
               all.x=TRUE,all.y=FALSE)

# Create case- and vote-level liberal outcome variables:

MasterC$LiberalDecision <- MasterC$decisionDirection-1
MasterC$LiberalDecision <- ifelse(MasterC$LiberalDecision==2,
                                  NA,MasterC$LiberalDecision)
MasterV$LiberalVote <- MasterV$direction-1

################
# Now, some regression models...
#
# Logistic regressions, *all* decisions / votes:

Vote.fit<-glm(LiberalVote~JusticeLiberalism+President+House+Senate,
              data=MasterV,family="binomial")

Decision.fit<-glm(LiberalDecision~JusticeLiberalism+President+House+
                    Senate,data=MasterC,family="binomial")

stargazer(Decision.fit,Vote.fit,type="text")

# LaTeX table:
# 
T1<-stargazer(Decision.fit,Vote.fit,type="latex",
              model.numbers=FALSE,
              dep.var.labels=c("Liberal Outcomes","Liberal Votes"))

# Next, separate the constitutional and statutory decisions:

MasterV$Const <- 0 # votes data...
MasterV$Const <- ifelse(MasterV$lawType==1,1,MasterV$Const)
MasterV$Const <- ifelse(MasterV$lawType==2,1,MasterV$Const)
MasterV$Stat <- 0
MasterV$Stat <- ifelse(MasterV$lawType==3,1,MasterV$Stat)
MasterV$Stat <- ifelse(MasterV$lawType==6,1,MasterV$Stat)
StatV <- MasterV[MasterV$Stat==1,] # statutory only
ConstV <- MasterV[MasterV$Const==1,] # constitutional only

MasterC$Const <- 0  # Cases data...
MasterC$Const <- ifelse(MasterC$lawType==1,1,MasterC$Const)
MasterC$Const <- ifelse(MasterC$lawType==2,1,MasterC$Const)
MasterC$Stat <- 0
MasterC$Stat <- ifelse(MasterC$lawType==3,1,MasterC$Stat)
MasterC$Stat <- ifelse(MasterC$lawType==6,1,MasterC$Stat)
StatC <- MasterC[MasterC$Stat==1,] # statutory only
ConstC <- MasterC[MasterC$Const==1,] # constitutional only

# More logistic regressions:

Vote.S.fit<-glm(LiberalVote~JusticeLiberalism+President+House+Senate,
              data=StatV,family="binomial")

Decision.S.fit<-glm(LiberalDecision~JusticeLiberalism+President+House+
                    Senate,data=StatC,family="binomial")

Vote.C.fit<-glm(LiberalVote~JusticeLiberalism+President+House+Senate,
                data=ConstV,family="binomial")

Decision.C.fit<-glm(LiberalDecision~JusticeLiberalism+President+House+
                      Senate,data=ConstC,family="binomial")

stargazer(Decision.S.fit,Decision.C.fit,Vote.S.fit,Vote.C.fit,
          type="text",model.numbers=FALSE,
          column.labels=c("Statutory","Constitutional","Statutory",
                          "Constitutional"))

# LaTeX:
# 
T2<-stargazer(Decision.S.fit,Decision.C.fit,Vote.S.fit,Vote.C.fit,
          type="latex",model.numbers=FALSE,
          column.labels=c("Statutory","Constitutional","Statutory",
                          "Constitutional"))

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Judicial Review...                                  ####
#
# Next: Read in data on SCOTUS judicial / constitutional 
# review of Congressional acts. The data are described 
# here:
#
# https://scholar.princeton.edu/kewhitt/publications/judical-review-congress-database

url <- "https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/judicial_review_of_congress_database_1789-2022.xlsx"
destfile <- "judicial_review_of_congress_database_1789_2022.xlsx"
curl::curl_download(url, destfile)
JR.df <- read_excel(destfile)
rm(url,destfile) # clean up

# Each observation is a single SCOTUS case/decision, in which
# the Court assessed and ruled on the constitutionality
# of a federal law/statute. We'll do some recoding of the
# variables therein, to make our next steps easier:

JR.df$Strike<-ifelse(JR.df$EFFECT=="struck down on face",1,0)
JR.df$Strike<-ifelse(JR.df$EFFECT=="struck down as face",1,JR.df$Strike)
JR.df$AsApplied<-ifelse(JR.df$EFFECT=="struck down as applied",1,0)
JR.df$Upheld<-ifelse(JR.df$EFFECT=="upheld",1,0)

# Next, we'll merge in the ideology of the House/Senate/
# President, by year:

JR.df<-merge(JR.df,NOM.ann,by=c("YEAR"),
           all.x=TRUE,all.y=FALSE)

# Note that we don't have ideology / Segal-Cover scores  
# for early Courts. We can include the mean Segal-Cover
# scores for that term/year, but only for judicial 
# review cases that have happened recently (post-1946).
# So, we'll merge in those data, but will fit separate 
# models with (1946-present) and without (1791-present)
# the Segal-Cover variable.

SCYear<-aggregate(JusticeLiberalism~term,data=MasterV,mean)
SCYear$YEAR<-SCYear$term
JR.df<-merge(JR.df,SCYear,by=c("YEAR"),all.x=TRUE,all.y=FALSE)

# Also: We need to *interact* the justices' ideology with
# that of the President and the Senate. Doing that is
# easier to understand if they are all "going in the same
# direction." Since both President and Senate ideology
# scores code higher values as more conservative, we'll
# recode the justices' mean Segal-Cover scores so that
# higher scores reflect greater conservatism as well:

JR.df$JConserv <- 1 - JR.df$JusticeLiberalism

# Now, some regression models:

JR1.fit<-glm(Strike~President+House+Senate,
             data=JR.df,family="binomial")

JR2.fit<-glm(Strike~President+House+Senate+JConserv+
               President*JConserv+House*JConserv+
               Senate*JConserv,
               data=JR.df,family="binomial")

stargazer(JR1.fit,JR2.fit,type="text")

# LaTeX:
# 
T2<-stargazer(JR1.fit,JR2.fit,type="latex",model.numbers=FALSE,
              column.labels=c("w/o Court Ideology",
                              "w/Court Ideology"))

# \fin