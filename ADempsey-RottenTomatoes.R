#================================================
# Data scrape from rotten tomatoes for ratings
# JSON api being used, code is similar to the NYT api extract
#================================================

# SET THE WORKING DIRECTORY TO WHAT IS APPROPRIATE FOR YOUR SYSTEM
setwd("/Users/adempsey/Documents/R/UCSC2612/FinalProject")


# SET YOUR FREE NY TIMES MOVIE API KEY HERE
my.key <- 'here it goes - here it goes - here it goes'

# lod the required packages
library(RJSONIO)
library(RCurl)
require(stringr)
require(plyr)

# extract the data
extract_details <- function(doc, movie, year){
  # this function needs to handle when it finds multiple movies....
  cat(movie,' - ',year,' - ',length(doc[[2]]),' matches\n',sep='')
  index <- 0
  if(length(doc[[2]]) == 0){
    dtls <- data.frame(title = movie,
                       year = year,
                       critics_rating = NA,
                       critics_score = NA,
                       audience_rating = NA,
                       audience_score = NA,
                       stringsAsFactors=FALSE)
  }else{
    for(i in 1:length(doc[[2]])){ # need to ignore case of text
      if(toupper(doc[[2]][[i]][2]) == toupper(movie) & doc[[2]][[i]][3] == year){  # must match title and year to avoid older dupes
        index <- i
      }
    if(index != 0){
      rat <- which(names(doc[[2]][[index]]) == "ratings")
      cr <- which(names(doc[[2]][[index]][[rat]]) == "critics_rating")
      cs <- which(names(doc[[2]][[index]][[rat]]) == "critics_score")
      ar <- which(names(doc[[2]][[index]][[rat]]) == "audience_rating")
      as <- which(names(doc[[2]][[index]][[rat]]) == "audience_score")
      dtls <- data.frame(title = movie,
                         year = year,
                         critics_rating = ifelse(length(cr) != 0, doc[[2]][[index]][[rat]][cr], NA),
                         critics_score = ifelse(length(cs) != 0, doc[[2]][[index]][[rat]][cs], NA),
                         audience_rating = ifelse(length(ar) != 0, doc[[2]][[index]][[rat]][ar], NA),
                         audience_score = ifelse(length(as) != 0, doc[[2]][[index]][[rat]][as], NA),
                         stringsAsFactors=FALSE)
      
    } else {
      dtls <- data.frame(title = movie,
                         year = year,
                         critics_rating = NA,
                         critics_score = NA,
                         audience_rating = NA,
                         audience_score = NA,
                         stringsAsFactors=FALSE)
    }
  }
  
  }
  # Not sure why the column names are being messed up wth the above code?
  names(dtls) <- c('title','year','critics_rating','critics_score','audience_rating','audience_score')  
  return(dtls)
}

# grabs the movie metadata from the API and then the review from the web page
grab_rotten_data <-function(movie){
  options(warn=-1)
  # 1: replace spaces with +
  movie.plus <- str_replace_all(movie[1],' ','+')
  # create a bad result if we have a scrape error
  bad.scrape <- data.frame(title = movie[1],
                           year = movie[2],
                           critics_rating = NA,
                           critics_score = NA,
                           audience_rating = NA,
                           audience_score = NA,
                           stringsAsFactors=FALSE)
  df <- bad.scrape # give it a default fist

  # 2: Build URL
  rottoms.url <- paste('http://api.rottentomatoes.com/api/public/v1.0/movies.json?q=',movie.plus,'&year=',movie[2],'&apikey=',my.key,sep='')
  
  rottoms.out <-getURLContent(rottoms.url,curl=getCurlHandle()) # this is the data
  doc <- fromJSON(rottoms.out) # return a list structure of the movies that match the title
  df <- extract_details(doc, movie[1],movie[2]) # extract the details I need
  if(nrow(df) == 0){ # if the exact movie was not returned, then return a bad scrape
    df <- bad.scrape
  }
  
  options(warn=1)
  return(df)
}  
###--- Main ---###


# Step 1: Read in the movie names
movie.file <- read.csv("BoxOffice.csv")

# Step 2: Grab only the movie names and set to character, and get the YEAR from the date
movie.names <- matrix(NA, nrow=nrow(movie.file),ncol=2)
movie.names[,1] <- as.character(movie.file[,1])
movie.names[,2] <- as.numeric(format(as.Date(movie.file[,7],'1970-01-01'),'%Y'))

# Step 3: Grab all the data ! - NOT DOING THE RIGHT APPLY CALL HERE?
movie.names <- as.data.frame(movie.names, stringsAsFactors=FALSE)
# handling two known issues
movie.names[1,1] <- '(500) Days of Summer'
movie.names[2,1] <- '(Untitled)'
rotten.df <- do.call("rbind", apply(movie.names, 1, grab_rotten_data))

# Step 4: write to file
# replacing 2 known issues
rotten.df[1,1] <- '500 Days of Summer'
rotten.df[2,1] <- 'Untitled'
write.csv(rotten.df ,file="RottenRatings.csv", row.names=FALSE)

# how many NA?
#sum(is.na(rotten.df$critics_rating)) 869
#sum(is.na(rotten.df$critics_score)) 765
#count(which(rotten.df$critics_score < 0)) 104
#sum(is.na(rotten.df$audience_rating)) 784
#sum(is.na(rotten.df$audience_score)) 765
#count(which(rotten.df$audience_score < 0)) 0

### code for manual investigations
#rottoms.url <- paste('http://api.rottentomatoes.com/api/public/v1.0/movies.json?q=burlesque&year=2010&apikey=',my.key,sep='')
#rottoms.out <-getURLContent(rottoms.url,curl=getCurlHandle()) # this is the data
#doc <- fromJSON(rottoms.out) # return a list structure of the movies that match the title
# doc[[2]][[1]][2]
#which(names(doc[[2]][[1]]) == "ratings")
