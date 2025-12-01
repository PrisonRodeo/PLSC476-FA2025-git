#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                ####
#
# This is some code from PLSC 476,
# dated December 2, 2025
#
# It contains a little bit of code for 
# grabbing the full script to the Coen 
# brothers' film "The Big Lebowski," and
# processing the text, creating a corpus
# object and a term-document matrix, and
# doing some basic / summary things with
# it.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load some useful R packages... (install as 
# necessary):

# install.packages("pdftools")
# install.packages("tm")
# install.packages("SnowballC")

library(pdftools)
library(tm)
library("SnowballC")


######################
# Read the script's text from a PDF that is 
# currently (as of 11/30/2025) available on the
# web:

TBL<-pdf_text("https://inwardeleven.com/lebowski/The%20Big%20Lebowski.pdf")

# Turn that into a "corpus" (a fancy list
# of around 138 items, each corresponding 
# to a page in the original script):

TBL.corp<-SimpleCorpus(VectorSource(TBL))

TBL.dtm <- DocumentTermMatrix(TBL.corp,
               control=list(removePunctuation=TRUE,
                            stopwords=TRUE,
                            tolower=TRUE,
                            stemming=TRUE,
                            removeNumbers=FALSE,
                            weight=weightTfIdf))

dim(TBL.dtm)
inspect(TBL.dtm)

# Show all terms (words) in the script that
# appear at least 30 times during the course
# of the film:
#
# findFreqTerms(TBL.dtm,lowfreq=30)
#
# Plot the occurrence of the word "rug," by 
# page of the script:

pdf("TBL-Rug-Count-25.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(tm_term_score(TBL.dtm,terms="rug"),xaxt="n",
        xlab="Script Page",ylab="Frequency of the word 'rug'")
axis(1,at=seq(0,140,by=20))
dev.off()

# Which words tend to appear most often on the
# same page of the script as the word "rug"?

Rug.cor<-findAssocs(TBL.dtm,terms="rug",corlimit=0.4)

pdf("RugCorrs-25.pdf",6,5)
par(mar=c(6,4,2,2))
plot(1:length(Rug.cor$rug),Rug.cor$rug,xaxt="n",
     pch=20,xlab="",ylab="Correlation")
axis(1,labels=FALSE,at=seq(1,length(Rug.cor$rug),by=1))
text(x=1:length(Rug.cor$rug)-1,
     y=par("usr")[3]-0.035,
     labels = names(Rug.cor$rug),
     xpd = NA,srt = 45)
dev.off()

# 
