#######################################
# This is some code from PLSC 476,
# "Empirical Legal Studies," dated 
# August 28, 2025.
#
# It provides a *very* basic introduction
# to R / RStudio. Ideally, this is
# review material for most students
# enrolled in the class.
#
# Note that anything on a line
# that starts with a "#" is a 
# comment, and will be ignored by
# the program (but should probably not 
# be ignored by you). It will also 
# show up in green text in the Source 
# window.
#######################################
#
# Basics: OBJECTS
#
# R is an object-oriented language, 
# which means that *objects* are its
# basic building blocks.
#
# Assign the number "6" to an object
# named "A":

A <- 6

# Now, whenever you refer to "A",
# you're referring to the number 6.

A  # returns "6" in the window below.
   # The square braces and [1] indicate
   # that this is a single value
   # (one row, one column)

A + 5  # returns "11" in the window below.

A^4    # returns "1296" (6x6x6x6).

"A" # prints "A". Using quotes means
    # "take me literally; actually return
    # whatever is in the quotes."

# If we want to get rid of A, we can remove 
# it:

rm(A)

# Objects can have multiple elements.
# Here is the PSU women's volleyball
# team's overall winning proportions, 
# 2005-2018:

PSUWVB<-c(31/34,32/35,34/36,38/38,38/38,
          32/37,25/32,33/36,34/36,36/39,
          28/34,24/34,33/35,25/32)

# The "c()" says "combine these values into
# a vector or list." Note the new object 
# called "PSUWVB" over there          ---->
# in the "Environment" window.        ---->
#
# We can list this object:

PSUWVB

# We can do transformations on it:

PSUWVB * 100

# Note that we now have numbers in the 
# square braces that indicate the position
# of each element (number) in the object;
# the first one is [1], the second is [2],
# etc. We can assign those transformations 
# to other objects:

WinPct <- PSUWVB * 100

# We can also combine objects; usually, we
# do this by creating a "data frame."
# Think of a data frame as like an Excel
# spreadsheet. 
#
# So, we might create another object that lists
# the years from 2005 to 2018, in order:

Year <- c(2005,2006,2007,2008,2009,
          2010,2011,2012,2013,2014,
          2015,2016,2017,2018)

Year

# Note that a faster way to do this is to 
# use the "seq()" (for "sequence") command:

Year <- seq(2005,2018)

# We just "overwrote" the old "Year" object
# with the new one. Note that R, in general,
# doesn't give you any "warning" when it
# overwrites objects.
#
# Now we can combine these two objects into
# a single data frame, using the
# "data.frame" command:

VB <- data.frame(Year = Year,
                 WinProp = PSUWVB,
                 WinPct = WinPct)

# Note that there's now a new data frame
# called "DF" in the Environment window;
# it has 14 observations (rows) and
# three variables (columns).
#
# NOTE: If you want to "see" a data frame,
# you can simply click on it in the
# Environment window; this has the same
# effect as running "View()" in the 
# console window:

View(VB)

# There's lots more to say about objects, but 
# that's enough for now. 
#
####################
# READING IN DATA
#
# Most of the time, you won't want to enter
# data by hand. Instead, you will read data
# from something like a spreadsheet into
# Rstudio from some other source file. There
# are many ways to do this.
#
# RStudio can read data in many different
# formats, but the simplest is ".csv" 
# ("comma-separated values"), which is
# simply an Excel-type spreadsheet that 
# has been converted into plain text, with
# "cells" of the spreadsheet separated 
# from each other by a comma.
#
# One way we can read a file is to have it
# "locally" on our own machine. To read 
# data locally, we have to start by telling
# R where to find the data. This can either
# be done via a "full" directory path, or
# by setting the "working" directory. For
# an example of the latter:

setwd("~/Dropbox (Personal)/PLSC 476")

# That set the "working" directory to the
# directory on my Macbook where the PLSC 476
# course materials are kept. I can then read some
# data like this:

SCOTUS <- read.csv("Data/SCOTUS-votes.csv")

View(SCOTUS)

# These data are on U.S. Supreme Court 
# justices who have served since 1946.
# The first column is the justice's name,
# the second is the percentage of cases
# involving civil rights and liberties in
# which s/he voted in a pro-civil rights
# direction, and the third column records 
# whether (=1) or not (=0) the justice
# was appointed by a Republican president.
#
# We can also read the same data directly
# from the web, using a URL for the file:

SCOTUS <- read.csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-FA2025-git/refs/heads/main/Data/SCOTUS-votes.csv")

# From there, we can do things like summaries:

summary(SCOTUS)

##########################
# SINGLING OUT ELEMENTS:
#
# A data frame is rows and columns. We can
# extract rows, columns, or cells by specifying
# which one we need using the square-brace
# ("[]") notation. Watch what happens when we
# print SCOTUS:

SCOTUS

# The notation "FOO[R,C]" denotes the Rth
# row and the Cth column of the object called
# "FOO". Note that R and C can be actual
# numbers, or statements that evaluate to
# numbers (or logicals). So if we wanted to 
# get Samuel Alito's row from the SCOTUS data,
# and we knew he was row #26, we could enter:

SCOTUS[26,]

# Alternatively, we could use something
# that evaluated to him, like:

SCOTUS[SCOTUS$justiceName=="SAAlito",]

# The fact that we left the second value
# (that is, the value after the comma) 
# blank means "give me all the columns
# from that row." Similarly, if we just 
# wanted the column with the justices' 
# voting percentages, we would enter:

SCOTUS[,2]

# which means "give me the values of 
# all the rows from column 2 of SCOTUS."
# A single value would then just be:

SCOTUS[26,3]

# which tells us that Alito was appointed 
# by a Republican. This is useful for a
# lot of things; for example, we can use
# it along with conditional statements to
# subset the data, like this:

GOPJustices <- SCOTUS[SCOTUS$GOP==1,]

# That means "Create an object called 
# 'GOPJustices' that consists of all of
# the columns of SCOTUS, but that only
# includes the rows that represent GOP
# appointees." We can see that the command
# worked:

View(GOPJustices)

#####################
# PLOTTING / GRAPHS
#
# R / RStudio is great for visualizing and
# plotting / graphing data. This is a fast
# introduction to the most basic graphing
# command, called "plot."
#
# A (very) basic plot of the volleyball team's
# winning percentage by year looks like this:

plot(VB$Year,VB$WinPct,t="l")

# The plot appears at the lower right.
# Note that the dollar-sign "XXX$YYY" 
# notation means "use the element YYY
# from object XXX." We can get around this
# in a few ways, the best probably being
# to use the "with()" command:

with(VB, plot(Year,WinPct,t="l"))

# This says "Use the object called 'VB'
# to draw a plot of the objects 'WinPct'
# and 'Year'." We can add a bunch of things
# to make it nicer looking:

with(VB, plot(Year,WinPct,t="l",lwd=2,col="navy",
      main="PSU Women's Volleyball Winning\nPercentages, 2005-2018",
      xlab="Season",ylab="Winning Percentage"))

# We can do a similar thing with the 
# Supreme Court data:

with(SCOTUS, boxplot(LiberalPct~GOP,
     ylab=c("Liberal Voting Percentage")))

# This is a (relatively bad) boxplot; it shows
# the range of values of the liberal voting 
# percentage for civil rights and liberties
# cases for Democratic-president-appointed
# (GOP=0) and Republican-president-appointed
# (GOP=1) SCOTUS justices.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# LOOPS AND BASIC PROGRAMMING                    ####
# 
# We'll do some work with simulations
# in this course. That means repetition,
# and that means loops. The basic structure
# of a loop in R is:
#
#  for (i in 1:N) {
#     <<activity>>
#  }
#
# This means "Take the activity, and repeat 
# it N times." Note that you don't need to
# use "i" specifically as the index.
#
# A simple example: we'll generate 100 
# "coin flips": draws of either heads (=1)
# or tails (=0) from a binary (bernoulli)
# distribution with P = 0.5:

set.seed(7222009)        # set seed
N <- 100                 # how many flips?
flips <- rbinom(N,1,0.5) # simulate

# (type "?rbinom" for details on this). We
# can summarize this by (e.g.) the number of
# "heads" out of 100:

sum(flips)             # tally up heads

# Now, suppose we wanted to repeat our
# flip-100-coins simulation many (say, 1000)
# times, and each time we want to record 
# the number of heads we get.
#
# First, we need to create a "container" to
# put the head-counts in, one with 1000 bins:

counts <- numeric(1000)   # count storage

# Next, we write a loop that repeats the 
# 100-flip simulation 1000 times, and each 
# time stores the count of "heads" as an
# element of "counts":

NSims <- 1000   # N of simulations
for(i in 1:NSims) {
  flips <- rbinom(N,1,0.5) # simulate
  counts[i] <- sum(flips)  # store count
}

# Now we can take a look at "counts":

summary(counts) # a summary of the simulated data
hist(counts)    # a histogram of the data
plot(density(counts)) # a density plot of the data

# Note that one can put loops inside of
# other loops. So, suppose I wanted to
# do the same simulation, but vary the
# number of coins I flipped (say, from
# 10 to 20 to 50 to 100 to 1000). I
# can do that all with a single set
# of nested loops:

NFlips <- c(10,20,50,100,1000) # N of flips
Sums <- data.frame(matrix(ncol=length(NFlips),
                          nrow=NSims))

for(j in 1:length(NFlips)) {
  for(i in 1:NSims) {
    flips <- rbinom(NFlips[j],1,0.5) # simulate
    Sums[i,j] <- sum(flips)  # store count
  }
}

# We can then summarize the results:

summary(Sums)

# ...and we could plot the results, etc.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# GETTING HELP                                   ####
#
# R has extensive documentation, and a help
# function that is available at the command
# line / console. The basic help syntax uses
# a question mark:

?summary

# Note that this will bring up a help file
# on any command that is part of a currently-
# loaded package. If the package is not loaded,
# you'll get an error:

?read.dta         # throws an error...

library(foreign)  # loads the "foreign" package
?read.dta         # no error!

# One can also do a omnibus search for help on
# a topic; this can be valuable if you know what
# you want to do, but don't know the package
# to do it with. That uses two question marks:

??spss

# The "??" command will work with almost anything:

??sunspots
??tuna
??Belgium
#
#
# Next time: Reviewing introductory 
# statistics...
