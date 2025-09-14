######################################################
# This is some code from PLSC 476, dated
# March 9, 2021.
#
# It's about law schools.
#####################################################
# Load a few necessary R packages [if you haven't
# installed these by now, you should do so, 
# using -install.packages()-]:

library(car)

# Set a couple options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also be sure to use -setwd()- to set your
# working directory (that is, where any files
# you create will be kept).
#
#####################################################
# Data are from here:
#
# https://docs.google.com/spreadsheets/d/1sHLKXAPP8icwEJK1VSJ3Wm25BVHVarfaB6zbZU1oaWk/edit?gid=0#gid=0
#
# ...and are for the year 2024.

LS <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/master/Data/LawSchools2024/LawSchools2024.csv")
LS$EmployScore<-as.numeric(LS$EmployScore)

# Next, make a scatterplot matrix of some key indicators
# of law schools:

pdf("LSScatterplotMatrix-25.pdf",8,6)
with(LS, 
     scatterplotMatrix(~LSAT+UGPA+Debt+MoreThanHalfScholarship+
                         FirstTimeBar+EmployScore,
                var.labels=c("Median LSAT Score","Median Undergrad\nGPA",
                "Average Debt","Percentage > Half\nScholarship",
                "First Time Bar\nPassage Rate","Employment Score"),
                col="black",pch=20,cex=0.7))
dev.off()


