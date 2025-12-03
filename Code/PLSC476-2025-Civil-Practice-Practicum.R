#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Introduction                                  ####
#
# This is some code from PLSC 476, dated 
# December 4, 2025.
#
# It contains code for scraping and 
# analyzing data from U.S. SEC litigation
# releases; a good landing page is:
#
# https://www.sec.gov/litigation/litreleases.htm
#
#=-=-=-=-=-=-=-=-=-=-=-=-=
# Load some useful R packages... (install as 
# necessary):

P<-c("httr","httr2","jsonlite","xml2","curl","rvest",
     "stringr","dplyr","lubridate","slam","tm")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# Run that ^ 7-8 times, as usual...
#
# Also be sure to -setwd()-:
#
# setwd("~/Dropbox/Enemies/Carrot Top")
# setwd("~/Dropbox (Personal)/PLSC 476/Notes and Slides")
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Data                                        ####
#
# Let's go get some data. We're going to focus on the year
# 2024, as it's the most recent year that is complete.
# Note as well that we're using a third-party aggregator:
#
# https://sec-api.io/docs/sec-litigation-releases-database-api
#
# ...and we're getting the data via an API. That means we
# need to have some code that will interface with the API;
# this is the function that bundles all that up:

Fetch_SEC_Litigation_2024 <- function(api_key) {
  
  # API endpoint
  url <- "https://api.sec-api.io/sec-litigation-releases"
  
  # Initialize results
  all_data <- list()
  from <- 0
  size <- 50
  total_fetched <- 0
  
  cat("Fetching SEC litigation releases from 2024 via API...\n")
  
  repeat {
    # Prepare request body
    body <- list(
      query = "releasedAt:[2024-01-01 TO 2024-12-31]",
      from = from,
      size = size,
      sort = list(list(releasedAt = list(order = "desc")))
    )
    
    # Make API request
    response <- POST(
      url,
      add_headers(Authorization = api_key),
      body = body,
      encode = "json",
      timeout(30)
    )
    
    # Check if request was successful
    if (status_code(response) != 200) {
      stop(sprintf("API request failed with status code: %d\n%s", 
                   status_code(response), 
                   httr::content(response, "text")))
    }
    
    # Parse response
    result <- httr::content(response, "parsed")
    
    # Add data to results
    all_data <- c(all_data, result$data)
    total_fetched <- total_fetched + length(result$data)
    
    cat(sprintf("Fetched %d of %d records\n", total_fetched, result$total$value))
    
    # Check if we've fetched all data
    if (total_fetched >= result$total$value || length(result$data) == 0) {
      break
    }
    
    # Update pagination
    from <- from + size
    
    # Be nice to the API
    Sys.sleep(0.5)
  }
  
  if (length(all_data) == 0) {
    stop("No data returned from API")
  }
  
  # Convert to dataframe
  lit_releases_df <- bind_rows(lapply(all_data, function(x) {
    data.frame(
      id = x$id %||% NA_character_,
      release_number = x$releaseNo %||% NA_character_,
      date = x$releasedAt %||% NA_character_,
      url = x$url %||% NA_character_,
      title = x$title %||% NA_character_,
      subtitle = x$subTitle %||% NA_character_,
      summary = x$summary %||% NA_character_,
      tags = paste(unlist(x$tags), collapse = "; "),
      entities = paste(sapply(x$entities, function(e) e$name %||% ""), collapse = "; "),
      has_settlement = x$hasAgreedToSettlement %||% NA,
      has_penalty = x$hasAgreedToPayPenalty %||% NA,
      stringsAsFactors = FALSE
    )
  }))
  
  # Convert date to proper format
  lit_releases_df$date <- as.POSIXct(lit_releases_df$date, 
                                     format = "%Y-%m-%dT%H:%M:%S",
                                     tz = "America/New_York")
  
  cat(sprintf("Successfully fetched %d litigation releases from 2024\n", 
              nrow(lit_releases_df)))
  
  return(lit_releases_df)
}

# Also: a helper function for null coalescing:

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Now, note that to run this code, you first need to acquire
# an API key (a long string of characters akin to a password).
# You can get that for free by signing up / signing in here:
#
# https://sec-api.io/docs/sec-litigation-releases-database-api
#
# Just click on the "Get a Free API Key" button in the 
# top-left, and then follow the instructions. The result will
# be your own API code, which you can substitute into the code
# below in place of the long one (one of mine) that is
# already there.
#
# Here's the command that goes out and uses the function
# above to grab the data from that API:

API_KEY <- "4a242ecbd6e6e5f839485b5e56d7edef8c91643b05b8ace3b13d3f4c0eda94e4"
SEC2024.df <- Fetch_SEC_Litigation_2024(API_KEY)

# To get that to work, you have to replace the API key
# string with your own API key.
#
# "SEC2024.df" now should be a data frame with 288 
# observations (one for each litigation release from
# 2024) and eleven variables characterizing each 
# release. Most of those are currently encoded as
# characters, which is fine for now.
#
# We can work with these data in the usual ways.
# Here's a histogram of release dates for
# the press releases:

pdf("SEC-Date-Histogram-25.pdf",6,5)
par(mar=c(4,4,2,2))
hist(SEC2024.df$date,breaks="months",freq=TRUE,
     col="grey",main="",ylim=c(0,50),
     xlab="Date",cex.axis=0.8)
dev.off()

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Corpus...                                     ####
#
# Now, turn that data frame -- and, specifically,
# the summaries -- into a -tm- corpus. To do
# that, we need to create a couple more variables:

SEC2024.df$doc_id<-SEC2024.df$id
SEC2024.df$text<-SEC2024.df$summary
SEC2024.df<-SEC2024.df[c("doc_id","text",setdiff(names(SEC2024.df),
                                         c("colA", "colB")))]

# Now make that a corpus:

SEC<-VCorpus(DataframeSource(SEC2024.df))
SEC

# Next, create a document-term matrix. Notice 
# that we're stemming / lemmatizing the words 
# here, and we're not retaining stopwords,
# capitalization, or numbers:

SEC.DTM<-DocumentTermMatrix(SEC,
             control=list(removePunctuation=TRUE,
                          tolower=TRUE,
                          stopwords=TRUE,
                          removeNumbers=TRUE,
                          stemming=TRUE))

rownames(SEC.DTM)<-SEC2024.df$release_number  # release IDs...
SEC.DTM  # print a description of this...

# Now, some basic "search"-type things. For example,
# if we want to see every single document that includes 
# any variant of the word "fraud" in it, we can do this:

frauds<-SEC.DTM[,grepl("fraud",SEC.DTM$dimnames$Terms)]
frauds
inspect(frauds)

# That gives us a DTM that only contains terms related
# to the word stem "fraud." 
#
# Which of those have the "most" focus on fraud?
# We can sort them by how often "fraud"-related
# terms appear in them:

table(row_sums(frauds))

frauds<-frauds[order(-row_sums(frauds)),]

pdf("SEC-Fraud-Barplot-25.pdf",7,5)
par(mar=c(6,4,2,2))
barplot(row_sums(frauds[1:28,]),ylim=c(0,4),
        ylab="Instances of 'fraud'",las=2)
dev.off()

# Next, we might want to use TF-IDF weights, to
# single out documents that are especially related to 
# fraud:

SEC.TFIDF<-weightTfIdf(SEC.DTM) #TF-IDF weighting 
SEC.TFIDF
inspect(SEC.TFIDF)

# Great. Now, which documents are the most
# "fraud"-y, based on their TF-IDF score?
# Make an new "frauds" object, but now using
# the TF-IDF scores:

frauds.tfidf<-SEC.TFIDF[,grepl("fraud",SEC.TFIDF$dimnames$Terms)]
inspect(frauds.tfidf)

frauds.tfidf<-frauds.tfidf[order(-row_sums(frauds.tfidf)),]

pdf("SEC-Fraud-Barplot-TFIDF-25.pdf",7,5)
par(mar=c(6,4,2,2))
barplot(row_sums(frauds.tfidf[1:12,]),
        ylab="TF-IDF for 'fraud'",las=2)
dev.off()

# We could do the same for the phrase "insider
# trading":

IT<-SEC.TFIDF[,grepl("scheme",SEC.DTM$dimnames$Terms)]
inspect(IT)

IT<-IT[order(-row_sums(IT)),]

pdf("SEC-IT-Barplot-TFIDF-25.pdf",7,5)
par(mar=c(4,4,2,2))
barplot(row_sums(IT[1:12,]),
        ylab="TF-IDF for 'insider'",las=2)
dev.off()

# /fin