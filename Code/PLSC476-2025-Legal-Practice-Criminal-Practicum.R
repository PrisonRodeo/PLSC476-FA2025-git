#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                ####
#
# This is some code from PLSC 476,
# dated November 20, 2025.
#
# The subject matter is predictive models,
# specifically as used in predicting
# offender recidivism, as used in RAIs.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Load necessary R packages, etc.             ####

P<-c("readr","tools","dplyr","ggplot2","psych",
     "readxl","stargazer","ROCR","pROC")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# ^ run that block 7-8 times until you get all smiley faces :)
#
# Set a couple options:

options(scipen = 3) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# And be sure to use -setwd- to set the local / 
# working directory to where you need it to be...

setwd("~/Dropbox (Personal)/PLSC 476/Notes and Slides")

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data                                        ####
#
#  The data we'll be using today is from the
# 2016 ProPublica study of racial disparaties
# in the COMPAS RAI. The piece is here:
#
# https://www.propublica.org/article/machine-bias-risk-assessments-in-criminal-sentencing
#
# and a summary of the data and methodology is
# here:
#
# https://www.propublica.org/article/how-we-analyzed-the-compas-recidivism-algorithm
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=
# To start, read some data:

DF<-read_csv("https://raw.githubusercontent.com/propublica/compas-analysis/master/cox-parsed.csv")
d2<-group_by(DF,id)
df<-summarise(d2,Name=paste0(first(first)," ",first(last)),
              Sex=first(sex),
              Age=max(age),
              Race=first(race),
              JuvFelonies=mean(juv_fel_count),
              JuvMisdem=mean(juv_misd_count),
              Priors=max(priors_count...15),
              ChargeDegree=first(c_charge_degree),
              COMPASScore=max(decile_score...12),
              COMPASRating=first(score_text),
              COMPASViolScore=max(v_decile_score),
              COMPASViolRating=first(v_score_text),
              Recid=max(is_recid),
              ViolentRecid=max(is_violent_recid))

rm(DF,d2)                     # <-- clean up...
df$Name<-toTitleCase(df$Name) # <-- capitalize names

# Fix some missingness:

df$COMPASScore<-ifelse(df$COMPASScore==-1,NA,df$COMPASScore)
df$COMPASViolScore<-ifelse(df$COMPASViolScore==-1,NA,
                           df$COMPASViolScore)

# Felony charge indicator:

felonies<-c("(F1)","(F2)","(F3)","(F4)","(F5)","(F6)","(F7)")
df$FelonyCharge<-ifelse(df$ChargeDegree%in%felonies,1,0)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Analysis                                        ####
#
# First, we'll build our own risk assessment 
# instrument. Following Lin et al. (2020), we'll 
# use logit (logistic regression) to explain /
# predict recidivism among the 10331 offenders in
# our sample.
#
# "Recid" is the variable that indicates whether (=0)
# or not (=1) each offender was a recidivist (that
# is, was rearrested) within two years. That is our
# "dependent variable" here. To start, we'll model
# that as a function of some demographic and 
# criminal history variables:

Recid.fit<-glm(Recid~Sex+Age+Race+JuvFelonies+JuvMisdem+
                     Priors+FelonyCharge,data=df,
                     family="binomial")
summary(Recid.fit)

# We can put that in a nice table:

Recid.Table<-stargazer(Recid.fit,type="text",model.numbers=FALSE,
              column.labels=c("Recidivism"))


# How well does that model predict recidivism 
# in-sample?

df$Recid.Probs<-predict(Recid.fit,type="response")
df$Recid.Preds<-ifelse(df$Recid.Probs>0.5,1,0)

conf<-xtabs(~Recid+Recid.Preds,data=df)
conf

((conf[1,1]+conf[2,2])/(sum(conf))) * 100 # Accuracy

# Calculate AUC-ROC:

Recid.ROC<-roc(df$Recid,df$Recid.Probs)
Recid.AUC<-auc(Recid.ROC)
Recid.AUC  # <-- about 0.70; not terrible, but not great either...

# ROC plot (with AUC-ROC statistic):

pdf("Recid-ROC-25.pdf",6,6)
par(mar=c(4,4,2,2))
plot(Recid.ROC,auc.polygon=TRUE,print.auc=TRUE,
     ci=FALSE,legacy.axes=TRUE,
     xlab="False Positive Rate",ylab="True Positive Rate")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Validation                                      ####
#
# That's great; now let's do some validation.
#
# First, "holdout" validation. We'll randomly 
# split the data into a "training" set (containing
# 90% of the data) and a "test" set (the other
# 10%). Then we'll fit the model to the training
# data, use the results to generate predictions
# on the test data, and examine predictive 
# power "out of sample." This is (arguably)
# the simplest form of validation.
#
# Create a variable to split the data on (note
# that there are lots of ways to do this; I'll
# use -sample-):

TrainingN <- round(0.9*nrow(df),0)
set.seed(7222009) # <-- set random number seed 
                  #     for replicability
insample<-sample(1:nrow(df),size=TrainingN)
df.Train<-df[insample,]  # Training subset
df.Test <-df[-insample,] # Test subset

# Re-fit the model, and generate predictions on
# the test data:

Train.fit<-glm(Recid~Sex+Age+Race+JuvFelonies+JuvMisdem+
               Priors+FelonyCharge,data=df.Train,
               family="binomial")
df.Test$Recid.Probs<-predict(Train.fit,newdata=df.Test,
                             type="response")
df.Test$Recid.Preds<-ifelse(df.Test$Recid.Probs>0.5,1,0)
conf2<-xtabs(~Recid+Recid.Preds,data=df.Test)

conf2

((conf2[1,1]+conf2[2,2])/(sum(conf2))) * 100 # Accuracy

# AUC-ROC and ROC curve plot:

Test.ROC<-roc(df.Test$Recid,df.Test$Recid.Probs)
Test.AUC<-auc(Test.ROC)
Test.AUC  # <-- about 0.72; again: not terrible

pdf("Test-ROC-25.pdf",6,6)
par(mar=c(4,4,2,2))
plot(Test.ROC,auc.polygon=TRUE,print.auc=TRUE,
     ci=FALSE,legacy.axes=TRUE,
     xlab="False Positive Rate",ylab="True Positive Rate")
dev.off()

# That's fine. For K-fold cross-validation, we
# do the same thing, but K times:
#
# 1. Divide the data into K random samples, each
#    with (1/K)*N observations,
# 2. Fit K separate models, holding out the {1,2,...K}th
#    subsample each time as test data and using the 
#    other K-1 subsamples as training data,
# 3. Each time, predict outcomes in the (kth) test
#    data set,
# 4. Calculate the OOS fit statistics for each of the
#    K test subsamples, and
# 5. Average over the K fit statistics to get the overall
#    OOS performance.
#
# There are lots of ways to do this, probably most
# easily by using the -caret- package. But in the 
# interest of transparency and pedagogy, we'll do 
# the cross-validation "by hand" here.
#
# We'll start by randomly splitting the data into 
# K=10 groups, and assign each group a number from
# 1 to 10:

K <- 10
N <- nrow(df)
set.seed(7222009) # <-- for replicability
df$SubsetID <- sample(rep(1:K,length.out=N))

# Now, write a loop* to do the steps outlined above:

AUCs<-numeric(K) # <-- a place to stash the AUCs...

for(i in 1:K) {
  dftrain<-df[df$SubsetID!=i,] # training data
  dftest <-df[df$SubsetID==i,] # test / validation data
  fit<-glm(Recid~Sex+Age+Race+JuvFelonies+JuvMisdem+
           Priors+FelonyCharge,data=dftrain,
           family="binomial")  # model...
  dftest$probs<-predict(fit,newdata=dftest,type="response")
  AUCs[i]<-auc(roc(dftest$Recid,dftest$probs,ci=TRUE))
}
rm(dftrain,dftest,fit) # <-- clean up

AUCs # <-- the K=10 AUC-ROC values...

# Plot those, along with their means:

pdf("KFold-AUCs-25.pdf",6,5)
par(mar=c(4,4,2,2))
barplot(AUCs,ylim=c(0,0.8),names.arg=seq(1:10),
        xlab="K",ylab="AUC-ROC")
abline(h=mean(AUCs),lwd=2,lty=2,col="red")
text(3,0.75,paste0("Mean AUC-ROC = ",
                   round(mean(AUCs),3)),col="red")
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# COMPAS Comparison                             ####
#
# Now, let's see for ourselves how well the 
# COMPAS scores do at predicting recidivism.
# To do this, we'll fit a logistic regression
# model that uses *only* the COMPAS scores /
# ratings to predict recidivism (since, in
# theory, the COMPAS ratings incorporate in-
# formation about all the other factors we
# used above):

CRate.fit<-glm(Recid~COMPASRating,data=df,
               family="binomial") # using COMPAS 3-cat rating
CScore.fit<-glm(Recid~COMPASScore,data=df,
               family="binomial") # using COMPAS 1-10 score
summary(CRate.fit)
summary(CScore.fit)

# Put those in a pretty table:

COMPAS.Table<-stargazer(CRate.fit,CScore.fit,type="text",
                        model.numbers=FALSE,
                        column.labels=c("COMPAS Ratings",
                                        "COMPAS Score"))

# Now, let's see how that model's predictive power
# looks. We'll once again K-fold cross-validate each
# of the models -- using the same random assignments
# that we used for our previous model's cross-
# validation -- and plot the results.
#
# First, the "ratings" models:

AUC.rate<-numeric(K) # <-- a place to stash the AUCs...

for(i in 1:K) {
  dftrain<-df[df$SubsetID!=i,] # training data
  dftest <-df[df$SubsetID==i,] # test / validation data
  fit<-glm(Recid~COMPASRating,data=dftrain,
           family="binomial")  # model...
  dftest$probs<-predict(fit,newdata=dftest,type="response")
  AUC.rate[i]<-auc(roc(dftest$Recid,dftest$probs,ci=TRUE))
}
rm(dftrain,dftest,fit) # <-- clean up

# Next, the "score" models:

AUC.score<-numeric(K) # <-- a place to stash the AUCs...

for(i in 1:K) {
  dftrain<-df[df$SubsetID!=i,] # training data
  dftest <-df[df$SubsetID==i,] # test / validation data
  fit<-glm(Recid~COMPASScore,data=dftrain,
           family="binomial")  # model...
  dftest$probs<-predict(fit,newdata=dftest,type="response")
  AUC.score[i]<-auc(roc(dftest$Recid,dftest$probs,ci=TRUE))
}
rm(dftrain,dftest,fit) # <-- clean up

# Now do a couple plots of those:

pdf("COMPAS-AUCs-25.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
# #1 (ratings):
barplot(AUC.rate,ylim=c(0,0.8),names.arg=seq(1:10),
        xlab="K",ylab="AUC-ROC (COMPAS Ratings)")
abline(h=mean(AUC.rate),lwd=2,lty=2,col="red")
text(4.5,0.75,paste0("Mean AUC-ROC = ",
                   round(mean(AUC.rate),3)),col="red")
# #2 (scores):
barplot(AUC.score,ylim=c(0,0.8),names.arg=seq(1:10),
        xlab="K",ylab="AUC-ROC (COMPAS Scores)")
abline(h=mean(AUC.score),lwd=2,lty=2,col="red")
text(4.5,0.75,paste0("Mean AUC-ROC = ",
                   round(mean(AUC.score),3)),col="red")
dev.off()

# The models that only include the COMPAS ratings
# / scores as predictors do slightly worse than our
# (still-pretty-simple) model that incorporates some
# of the data's predictors...
#
# Footnotes:
#
# * We could use an -apply- function of some kind
#   here instead of a loop, but loops seem more 
#   intuitive, and aren't any slower.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Digression: ROC plot examples (slide 9)...  #### 

M<-1000
set.seed(7222009)
Y1<-rbinom(M,1,0.5)
P1<-numeric(M)
P1<-ifelse(Y1==0,runif(507,min=0.01,max=0.49),
           runif(493,min=0.51,max=0.99)) # Perfect pred.
set.seed(1337)
Y2<-rbinom(M,1,0.5)
P2<-numeric(M)
P2<-ifelse(Y2==0,rbeta(518,2,3),
           rbeta(482,3,2))       # Imperfect pred.
set.seed(11101968)
Y3<-rbinom(M,1,0.5)
P3<-numeric(M)
P3<-runif(M,min=0.001,max=0.999) # Coin flip

# Now plots:

pdf("ROC-Examples-25.pdf",11,4)
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
plot(roc(Y1,P1),auc.polygon=TRUE,print.auc=TRUE,
     ci=FALSE,legacy.axes=TRUE,print.auc.cex=1.5,
     xlab="False Positive Rate",ylab="True Positive Rate",
     main="Perfect Prediction")
points(1,1,pch=19,cex=2)
plot(roc(Y2,P2),auc.polygon=TRUE,print.auc=TRUE,
     ci=FALSE,legacy.axes=TRUE,print.auc.cex=1.5,
     xlab="False Positive Rate",ylab="True Positive Rate",
     main="Good Prediction")
plot(roc(Y3,P3),auc.polygon=TRUE,print.auc=TRUE,
     ci=FALSE,legacy.axes=TRUE,print.auc.cex=1.5,
     xlab="False Positive Rate",ylab="True Positive Rate",
     main="Zero Prediction")
dev.off()
