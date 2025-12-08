#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                    ####
#
# This is some code from PLSC 476, dated 
# December 9, 2025.
#
# It contains code for analyzing some
# AmLaw 200-related data from 2015.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load some useful R packages... (install as 
# necessary):

library(RCurl)
library(readr)
library(stargazer)

#=-=-=-=-=-=-=
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 6) # show fewer decimal places

# Also be sure to -setwd()-:
#
# setwd("~/Dropbox/Fun Stuff/Genshin Impact")
# setwd("~/Dropbox (Personal)/PLSC 476/Notes and Slides")
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data                                            ####
#
# Let's read some data. These are top-line 
# financials for the AmLaw 200 (the 200 largest
# law firms in the U.S., based on gross revenue).
# They are from 2015 (to keep me from getting into
# trouble with ALM, the entity that creates the 
# data and puts it behind a paywall), but it 
# gives a good sense of what the business land-
# scape for the "top" law firms looks like.

df<-read_csv("https://github.com/PrisonRodeo/PLSC476-FA2025-git/raw/refs/heads/main/Data/AmLaw200-2015.csv")
df<-as.data.frame(df) # get rid of the "tibble" business
df<-df[order(df$Rank2015),]  # sort the data...
rownames(df)<-df$Firm  # label the rows with firm names

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Diamonds and Pyramids                           ####
#
# First, let's talk about the different "models" for
# law firms (pyramids vs. diamonds). A firm that is a 
# "diamond" has a high non-equity-partner-to-equity-partner 
# ratio; it also sometimes has few (or no) non-equity 
# partners (i.e., the numerator in the numerator of the 
# leverage statistic is the number of associates).
#
# A "diamond" firm has a relatively low number of 
# equity parntners, (relatively) few associates, and
# more non-equity partners and other "staff
# attorneys." It's leverage may also be high or
# low.
#
# The number of associates is equal to the number of
# lawyers, minus the number of equity and non-equity
# partners:

df$NAssociates<-df$NLawyers - (df$NEquityPartners+df$NNonEquityPartners)
summary(df$NAssociates)

# A "pyramid" firm:

df[df$Firm=="Skadden, Arps, Slate, Meagher & Flom",c(12:14,16,21)]

# A "diamond" firm:

df[df$Firm=="Lewis Brisbois Bisgaard & Smith",c(12:14,16,21)]

# Now let's make some plots. First, revenue in the
# "top 20":

pdf("AmLawTop20-Revenue.pdf",7,5)
par(mar=c(4,16,2,2))
barplot(df$GrossRevenue[1:20]/1000000,
        xlab="Gross Revenue (in $millions, 2015)",
        names.arg=df$Firm[1:20],horiz=TRUE,las=1,
        cex.names=0.8)
abline(v=c(500,1000,1500,2000,2500),lty=2)
dev.off()

# Now revenue for all 200...

pdf("AmLaw200-Revenue.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(df$GrossRevenue/1000000,
        xlab="Firm Rank",
        ylab="Gross Revenue (in $millions, 2015)")
axis(1,at=c(1,120,240),labels=c(1,100,200))
abline(h=c(500,1000,1500,2000),lty=2)
dev.off()

# Gross revenue is deceiving, both because of different
# firm sizes and because of differing costs (e.g., 
# compensation). Let's plot some alternative measures.
# First, we'll look at revenues per lawyer:

df<-df[order(-df$RevenuePerLawyer),] # sort the data...

pdf("AmLaw200-RPL.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(df$RevenuePerLawyer/1000000,
        xlab="Firm Rank",
        ylab="Revenue Per Lawyer (in $millions, 2015)")
axis(1,at=c(1,120,240),labels=c(1,100,200))
abline(h=c(0.5,1,1.5,2),lty=2)
text(0,2.5,df[1,]$Firm,pos=4)
dev.off()

# Compare firm size and revenue...

r<-with(df,cor(log(GrossRevenue),log(NLawyers)))

pdf("AmLawRevSizeScatter.pdf",6,5)
with(df, plot(GrossRevenue/1000000,NLawyers,log="xy",pch="",
              xlim=c(50,5000),ylim=c(100,5000),
              ylab="Number of Lawyers (log scale)",
              xlab="Gross Revenue in $Millions (log scale)"))
with(df, text(GrossRevenue/1000000,NLawyers,log="xy",
              labels=df$Firm,cex=0.5))
text(80,3000,paste0("r = ",round(r,3)))
dev.off()

# That deals with firm size, but not expenses.
# To get at that we can examine profit margins:
# the ratio of total profits to total revenue:

df<-df[order(-df$ProfitMargin),] # sort the data...
meanPM<-mean(df$ProfitMargin)

pdf("AmLaw200-ProfitMargin.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(df$ProfitMargin,
        xlab="Firm Rank",
        ylab="Profit Margin (percentage)")
axis(1,at=c(1,120,240),labels=c(1,100,200))
abline(h=meanPM,lty=2)
legend("topright",bty="n",lwd=1,lty=2,
       legend="Mean Profit Margin")
dev.off()


# Finally, let's look at profits per (equity) 
# partner (PPP):

df<-df[order(-df$ProfitsPerEquityPartner),]

pdf("AmLaw200-PPP.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(df$ProfitsPerEquityPartner/1000000,
        xlab="Firm Rank",
        ylab="Profits Per Equity Partner (in $millions, 2015)")
axis(1,at=c(1,120,240),labels=c(1,100,200))
abline(h=c(1,2,3,4,5),lty=2)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Modeling profits                                ####
#
# What are the things that are associated with
# higher profits per partner? We can examine a 
# few: firm size (number of partners, and number
# of lawyers overall), gross revenue, and leverage:

r1<-with(df,cor(log(ProfitsPerEquityPartner),log(NEquityPartners)))
r2<-with(df,cor(log(ProfitsPerEquityPartner),log(NLawyers)))
r3<-with(df,cor(log(ProfitsPerEquityPartner),log(GrossRevenue)))
r4<-with(df,cor(log(ProfitsPerEquityPartner),log(Leverage)))

pdf("AmLaw200-PPP-Scatters.pdf",10,8)
par(mfrow=c(2,2))
par(mar=c(4,4,2,2))
with(df, plot(NEquityPartners,ProfitsPerEquityPartner/1000000,
        log="xy",pch=20,
        xlab="Number of Equity Partners",
        ylab="Profits Per Equity Partner (in $millions, 2015)"))
text(700,5,paste0("r = ",round(r1,3)))
par(mar=c(4,4,2,2))
with(df, plot(NLawyers,ProfitsPerEquityPartner/1000000,
              log="xy",pch=20,
              xlab="Number of Lawyers",
              ylab="Profits Per Equity Partner (in $millions, 2015)"))
text(2500,5,paste0("r = ",round(r2,3)))
par(mar=c(4,4,2,2))
with(df, plot(GrossRevenue/1000000,ProfitsPerEquityPartner/1000000,
              log="xy",pch=20,
              xlab="Gross Revenue (in $millions)",
              ylab="Profits Per Equity Partner (in $millions, 2015)"))
text(150,5,paste0("r = ",round(r3,3)))
par(mar=c(4,4,2,2))
with(df, plot(Leverage,ProfitsPerEquityPartner/1000000,
              log="xy",pch=20,
              xlab="Leverage",
              ylab="Profits Per Equity Partner (in $millions, 2015)"))
text(1,5,paste0("r = ",round(r4,3)))
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=
# Now, we'll classify firms as pyramids or diamonds.
# Some decision rules:
#
# 1. If a firm has zero non-equity partners, it's
#    automatically a pyramid;
#
# 2. If a firm has more non-equity partners than
#    associates, it's automatically a diamond;
#
# 3. If a firm has more than zero non-equity 
#    partners, but fewer non-equity partners than
#    associates, then:
#    A) it will be classified as a pyramid if the ratio
#       of non-equity partners to associates is less
#       than 0.5 (that is, if there are at least two
#       associates for every non-equity partner),
#    B) it will be classified as a diamond if the ratio
#       of non-equity partners to associates is greater
#       than 0.5 (that is, if there are fewer than two
#       associates for every non-equity partner).

df$Diamond<-NA
df$Diamond<-ifelse(df$NNonEquityPartners==0,0,df$Diamond)
df$Diamond<-ifelse(df$NNonEquityPartners>df$NAssociates,1,df$Diamond)
df$Diamond<-ifelse(df$NNonEquityPartners/df$NAssociates<0.5,0,df$Diamond)
df$Diamond<-ifelse(df$NNonEquityPartners/df$NAssociates>0.5,1,df$Diamond)

# Now, let's examine differences in PPP across the two
# types of firms:

df$DLabel<-ifelse(df$Diamond==0,paste0("Pyramid"),paste0("Diamond"))
Type.t<-t.test(log(ProfitsPerEquityPartner)~Diamond,data=df)

pdf("PPP-By-Firm-Type.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, boxplot(ProfitsPerEquityPartner/1000000 ~ DLabel,
                 log="y",xlab="Firm Type",notch=TRUE,
                 ylab="Profits Per Equity Partner (in $millions)"))
legend("topleft",bty="n",legend=c(paste0("t = ",round(Type.t$statistic,3)),
                                  paste0("P < ",round(Type.t$p.value,3)+0.01)))
dev.off()

# OK, cool. Note that the same thing is true for profit
# margins:
#
# t.test(ProfitMargin~Diamond,data=df)
#
# Now, see if that effect persists once we control for other
# factors:

all.fit<-lm(log(ProfitsPerEquityPartner)~log(GrossRevenue)+
              log(NLawyers)+log(Leverage)+Diamond,data=df)

# ... or if factors such as firm size and leverage operate
# differently in pyramid vs. diamond firms:

P.fit<-lm(log(ProfitsPerEquityPartner)~log(GrossRevenue)+
            log(NLawyers)+log(Leverage)+Diamond,
            data=df[df$Diamond==0,])

D.fit<-lm(log(ProfitsPerEquityPartner)~log(GrossRevenue)+
            log(NLawyers)+log(Leverage)+Diamond,
          data=df[df$Diamond==1,])

# Make a nice table:

stargazer(all.fit,P.fit,D.fit,type="text",
          intercept.bottom=FALSE,
          column.labels=c("All Firms","Pyramid Firms",
                           "Diamond Firms"),
          model.numbers=FALSE)
