#================================================
# Data scrape from New York Times for movie ratings and movie review text
# NYT movie API is used to get movie metadata and then an HTML scrape is used to grab the review text
#
# Answered with the help of Tony Breyal through StackOverflow - Thank you
#  http://stackoverflow.com/questions/9320525/unable-to-pull-text-out-of-a-scraped-html-page-with-r-xml-package
#================================================

# SET THE WORKING DIRECTORY TO WHAT IS APPROPRIATE FOR YOUR SYSTEM
setwd("/Users/adempsey/Documents/R/UCSC2612/FinalProject")

# SET YOUR FREE NY TIMES MOVIE API KEY HERE
my.key <- 'here it goes - here it goes - here it goes'

# load package
require(XML)
require(stringr)
# functions

# extract the data
extract_details <- function(doc){
  dtls <- data.frame(title = xpathSApply(doc, "//results//display_title", xmlValue), 
             critics.pick = xpathSApply(doc, "//results//critics_pick", xmlValue),
             thousand.best = xpathSApply(doc, "//results//thousand_best", xmlValue),
             opening.date = xpathSApply(doc, "//results//opening_date", xmlValue),
             url = xpathSApply(doc, "//results//link[@type='article']/url", xmlValue),
             stringsAsFactors=FALSE)
  return(dtls)
}

# grabs the movie metadata from the API and then the review from the web page
grab_nyt_data <-function(movie){
  options(warn=-1)

  # 1: replace spaces with +
  movie.plus <- str_replace_all(movie,' ','+')
  
  # create a bad result if we have a scrape error
  bad.scrape <- data.frame(title = movie, 
             critics.pick = 0,
             thousand.best = 0,
             opening.date = '12/31/2011',
             url = 'did not get',
             text = 'did not get',
             stringsAsFactors=FALSE)
  df <- bad.scrape # give it a default fist
  
  # 2: Build URL
  url <- paste('http://api.nytimes.com/svc/movies/v2/reviews/search.xml?query=',movie.plus,'&api-key=',my.key,sep ='')

  # 3: Main scraping
  tryCatch({
    doc <- xmlParse(url) # parse the xml
    df <- extract_details(doc) # extract the details I need
    df <- df[df$title == movie, ] # filter movies down to the exact one I want
    if(nrow(df) == 0){ # if the exact movie was not returned, then return a bad scrape
      df <- bad.scrape
    }
    free(doc) # dump the doc object
    if(df$url != 'did not get'){ # if I didnt get the exat movie I dont want to review
      df$text <- sapply(df$url, grab_nyt_text)
    } else {
      df$text <- 'did not get'
    }
  },error=function(e){
    cat('failed API for ',movie,'\n')
    })

  options(warn=1)
  # last place to capture bad scrape
  if(length(df) == 1){df <- bad.scrape}
  return(df)
}

# grabs text from new york times movie review page. 
grab_nyt_text <- function(u) {
  tryCatch({
    doc <- htmlParse(u)
    txt <- xpathSApply(doc, '//div[@class="articleBody"]//p', xmlValue)
    txt <- paste(txt, collapse = "\n")
    free(doc)
  },error=function(e){
    txt <- 'did not get'
  })
  return(txt)
}


###--- Main ---###

# Step 1: Read in the movie names
movie.file <- read.csv("BoxOffice.csv")

# Step 2: Grab only the movie names and set to character
movie.names <- as.character(movie.file[,1])

# Step 3: Grab all the data !
nyt.df <- do.call("rbind", lapply(movie.names, grab_nyt_data))

# Step 4: write to file
write.csv(nyt.df ,file="NYTimes.csv", row.names=FALSE)

##--- SECONDARY FOR FAILED SCRAPES ---###

# Step 5: Read in the movie names
bad.movie.file <- read.csv("NYTimes.csv", stringsAsFactors=FALSE)

# Step 6: Grab the bad movie names and the good movie data
temp <- bad.movie.file$url == 'did not get'
bad.movie.names <- as.character(bad.movie.file[temp,1])
good.movie.file <- bad.movie.file[!temp,]
# put the good movies back into a file, just to be safe
write.csv(good.movie.file ,file="NYTimesGood1.csv", row.names=FALSE)
rm(bad.movie.file)

# Step 7, put the now good movies with the rest of the good movies and try another scrape for the remaining bad
# multiple scrapes in fact, agains, due to problems with http requests
bad.nyt.df <- do.call("rbind", lapply(bad.movie.names, grab_nyt_data))
temp <- bad.nyt.df$url == 'did not get'
good.movie.file <- rbind(good.movie.file, bad.nyt.df[!temp,])
write.csv(good.movie.file ,file="NYTimesGood2.csv", row.names=FALSE)

bad.movie.names <- as.character(bad.nyt.df[temp,1])
bad.nyt.df <- do.call("rbind", lapply(bad.movie.names, grab_nyt_data))
temp <- bad.nyt.df$url == 'did not get'
good.movie.file <- rbind(good.movie.file, bad.nyt.df[!temp,])
write.csv(good.movie.file ,file="NYTimesGood3.csv", row.names=FALSE)

bad.movie.names <- as.character(bad.nyt.df[temp,1])
bad.nyt.df <- do.call("rbind", lapply(bad.movie.names, grab_nyt_data))
temp <- bad.nyt.df$url == 'did not get'
good.movie.file <- rbind(good.movie.file, bad.nyt.df[!temp,])
write.csv(good.movie.file ,file="NYTimesGood4.csv", row.names=FALSE)

bad.movie.names <- as.character(bad.nyt.df[temp,1])
bad.nyt.df <- do.call("rbind", lapply(bad.movie.names, grab_nyt_data))
temp <- bad.nyt.df$url == 'did not get'
good.movie.file <- rbind(good.movie.file, bad.nyt.df[!temp,])
write.csv(good.movie.file ,file="NYTimesGood5.csv", row.names=FALSE)

bad.movie.names <- as.character(bad.nyt.df[temp,1])
bad.nyt.df <- do.call("rbind", lapply(bad.movie.names, grab_nyt_data))
temp <- bad.nyt.df$url == 'did not get'
good.movie.file <- rbind(good.movie.file, bad.nyt.df[!temp,])
write.csv(good.movie.file ,file="NYTimesGood6.csv", row.names=FALSE)

# some manual edits to the final excel file will be required - some titles have 2 records, one with data and one without, mostlikely due to rereleases from IMDB and single reviews in NYTimes
# additionally, the final file needs to be opened up by a tool such as "Text Wrangler", in order to easily remove non-ascii, control and null characters with ease