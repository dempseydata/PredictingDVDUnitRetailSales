#=============================================
# Data scrape from box office mojo for all titles
# and then filtering down to 2009-2011 only
#
# Code originally from the URL below, tweaked to perform required task
# http://tonybreyal.wordpress.com/2012/01/13/r-a-quick-scrape-of-top-grossing-films-from-boxofficemojo-com/
#=============================================

# SET THE WORKING DIRECTORY TO WHAT IS APPROPRIATE FOR YOUR SYSTEM
# setwd("/Users/adempsey/Documents/R/UCSC2612/FinalProject")

# load required packages
require(XML)

# local helper functions
get_table <- function(u) {
  # we need the 7th table in the page source
  # BUT, the first row in the table only has 5 columns (for headings)
  # whilst the rest of the rows have 7
  table <- readHTMLTable(u, skip.rows=1,as.data.frame=TRUE)[[7]]
  names(table) <- c("Title", "Studio", "Domestic.Gross", "Domestic.Theaters", "Opening.Gross", "Opening.Theaters", "Open.Date")
  table <- as.data.frame(table, stringsAsFactors=FALSE) # get rid of factors and force strings for cleaning purposes
  return(table)
}

# clean a data frame, by cleaning each column
clean_df <- function(df) {
  df <- sapply(df, clean)
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  return(df)
}

# clean a single column by removing punctuation
clean <- function(col) {
  col <- gsub("$", "", col, fixed = TRUE)
  col <- gsub("%", "", col, fixed = TRUE)
  col <- gsub(",", "", col, fixed = TRUE)
  col <- gsub("^", "", col, fixed = TRUE)
  col <- gsub("'", "", col, fixed = TRUE)
  col <- gsub("(", "", col, fixed = TRUE)
  col <- gsub(")", "", col, fixed = TRUE)
  col <- gsub(":", "", col, fixed = TRUE)
  col <- gsub("&", "", col, fixed = TRUE)
  col <- gsub("?", "", col, fixed = TRUE)
  col <- gsub("  ", " ", col, fixed = TRUE) # double spaces down to 1
  col <- gsub("n/a", NA, col,fixed = TRUE) # replace with mising values
  col <- gsub("N/A", NA, col,fixed = TRUE) # replace with missing values
  return(col)
}

# Step 1: define the necessary arrays for URL construction
# Define the index values used in the URL
letter.pages <- c('NUM','A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z')
# Define the number of pages each index section has - a total of 135 pages in all
num.pages <- c(1,8,8,7,6,5,6,6,6,5,3,3,5,6,5,5,7,1,6,13,8,2,3,6,1,2,1)

# Step 2: construct URLs
# create an empty array of length 135
urls <- array(" ", 135)

for(m in 1:length(letter.pages)) { # for each of the letter.pages
  for(n in 1:num.pages[m]){ # for each page of the letter
    if(n ==1){ # the first page of a letter, does not need the page variable in the URL
      # stick the new URL in the slot that is the sum of all previous pages, plus the iteration of the current number page of the current letter
      urls[sum(num.pages[1:m-1]) + n] <- paste("http://boxofficemojo.com/movies/alphabetical.htm?letter=",letter.pages[m],"&p=.htm", sep = "")
    } else {
      urls[sum(num.pages[1:m-1]) + n] <- paste("http://boxofficemojo.com/movies/alphabetical.htm?letter=",letter.pages[m],"&page=",n,"&p=.htm", sep = "")
    }
  }
}

# Step 3: scrape website
# the following code generates a unicode error which I cannot work out how to fix
# so I am disabling warnings for this part only
options(warn=-1)
df <- do.call("rbind", lapply(urls, get_table))
options(warn=1)

# Step 4: clean dataframe
df <- clean_df(df)
  
# Step 5: set column types
df[, c(3:6)] <- sapply(df[, c(3:6)], as.numeric)
df[, 7] <- sapply(df[, 7], as.Date,'%m/%d/%Y') # converts to days since 1/1/1970
  
# Step 6: Remove all titles that are not in 2009, 2010 or 2011
df <- subset(df,df$Open.Date > as.Date("12/31/2008",'%m/%d/%Y'))
df <- subset(df, df$Open.Date < as.Date("01/01/2012", '%m/%d/%Y'))

# Step 6: remove entries with NA box office domestic (3) and opening weekend (5)
# I can always remove records without num.theaters later
df <- df[!is.na(df[,3]),]
df <- df[!is.na(df[,5]),]

# Step 7: Save data frame to csv file
write.csv(df ,file="BoxOffice.csv", row.names=FALSE)
