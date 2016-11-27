#================================================
# Merge all source data together, into a single movie data file
#
# 
#================================================

# SET THE WORKING DIRECTORY TO WHAT IS APPROPRIATE FOR YOUR SYSTEM
setwd("/Users/adempsey/Documents/R/UCSC2612/FinalProject")

# read in required packages
require(plyr)
require(ggplot2)
require(DMwR)  # from "Data Mining with R LEarning by Case Studies" book

# read in the source data
box.office <- read.csv("BoxOffice.csv", stringsAsFactors=FALSE)
sentiment <- read.csv("NYTimes-Sentiment.csv", stringsAsFactors=FALSE)
rotten <- read.csv("RottenRatings.csv", stringsAsFactors=FALSE)
dvd.sales <- read.csv("DVDSales.csv", stringsAsFactors=FALSE)

# rename columns for easier joining
names(sentiment) <- c("Title", "critics.pick", "thousand.best", "opening.date", "sentiment.score")
names(rotten) <- c("Title", "year", "critics.rating", "critics.score", "audience.rating", "audience.score")


# Clean join between box office and rotten tomatoes
# Keep all of BO/RT records when joining to sentiment
# Thsi way, we can deal with missing values later, either through exclusion or KNN imputation
merge1 <- join(rotten, box.office, by = "Title", type='inner')  # this is a clean join
merge1 <- join(merge1, sentiment,  by = "Title", type='left') # keep all records from merge 1
# drop the surplus data sets
rm(sentiment)
rm(box.office)
rm(rotten)

# Try to do the same for merge1 + dvd.sales
# but need to do something else due to case differences and differences in titles
# mereg1 = camel case, dvd.sales = upper case
# merge1 = "The A-Team", dvd.sales = "A-TEAM"
# so, we will build up a list of arrays, that contain possible matches for the strings in merge1

# create an empty possible.matches list
# hard to preallocate, as we dont know the length of the various sublists, but we know there will be 888 components though
possible.matches <- vector('list', nrow(merge1))

for(i in 1:nrow(merge1)){
  # assume dvd.sales has the shoter titles
  # can't flip flop search
  a <- grep(merge1[i,1],dvd.sales[,1], ignore.case = TRUE, value=FALSE)
  possible.matches[[i]] <- a # no match was found
}

# assume that all matches with a length 1 are good, so long as the value is not 0
good.match <- array(0, dim=nrow(merge1))

#================
# for some reason, cannot just 'source' execute?
for(i in 1:nrow(merge1)){
  if(length(possible.matches[[i]]) == 1){ # single length list, one or none found
    if(possible.matches[[i]] >= 1){ # single match found
      # single match found
      good.match[i] <- possible.matches[[i]][1]
      possible.matches[[i]] <- 0
    } else { # no matches found
      possible.matches[[i]] <- 0
    }
  } else if(length(possible.matches[[i]]) > 1){
    # lets print all the options, and ask for an input to identify which one to use
    cat('The source title is record:',i,' and title:',merge1[i,1],'which of the following is the true match? (0 for none) \n')
    for(j in 1:length(possible.matches[[i]])){  # print out all matching titles
      cat('\t\t record: ',possible.matches[[i]][j],' with title:',dvd.sales[possible.matches[[i]][j],1],'\n' )
    }
    cat('\t\t\t\t\t\tPlease Enter the ID of the title to use for matching, or 0 if there is not match:')
    good.match[i] <- as.numeric(readLines(con = stdin(), n = 1, ok = FALSE))
    possible.matches[[i]] <- 0
    # i am NOT trapping any errors here, which I should ifg I was building a more robust solution
    # since I am using this only once, then it is not too bad.
  }
}


# now flip the search and go through dvd.sales row by row, comparing with subset of merge1
# when potential matches are found, DISPLAY the results and ask for input
# 0 response basically means keep looking, other response means to insert the i loop number (dvd sales record number) into the appropriate slot of the matches.
missing <- array(0, sum(ifelse(good.match == 0,1,0)))
j = 1
for(i in 1:length(good.match)){
  if(good.match[i] == 0){
    missing[j] <- i
    j <- j + 1
  }
}

# search through dvd.sales, for matches to unmatched titles in merge1
# SKIP any already matched records
for(i in 1:nrow(dvd.sales)){
  
    # this gives TRUE or NA? Why cant I get false with the ! there?
    
  if(!any(good.match == i)){  # this dvd.sales record is not matched, yet, so lets check it
    a <- agrep(dvd.sales[i,1],merge1[missing,1], ignore.case = TRUE, value=FALSE) # returns the index of MISSING, not the index of merge
    if(length(a) > 0){  # there is a match, so present the information
      # what have we found
      cat('The dvd sales title is record:',i,' and title:',dvd.sales[i,1],'Should it match any of the following? (0 for none) \n')
      for(j in 1:length(a)){  # print out all matching titles
        cat('\t\t record: ',missing[a[j]],' with title:',merge1[missing[a[j]],1],'\n' )
      }
      cat('\t\t\t\t\t\tPlease Enter the ID of the title to use for matching, or 0 if there is not match:')
      slot <- as.numeric(readLines(con = stdin(), n = 1, ok = FALSE))
      if(slot > 0){ # I have stated a match
        good.match[slot] <- i # set the slot that was entered, to the string that was searched for
      }
    }
  }
}

# how many matches am I missing? 676, so I have 907 matched recors sfor DVD sales
# sum(ifelse(good.match == 0,1,0))
# merge 1 = 16 columns, want all except column 15
merge.cols = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,16)
# dvd sales = 4 cols, only want last 3
dvd.cols = c(2,3,4)
#Total columns = 18
#Total rows = 907
# these two are the same entry
#good.match[good.match != 0][872]
#good.match[1583]

final.movie.match <- as.data.frame(matrix(NA, nrow=907, ncol=18))
df.row = 1
for(i in 1:length(good.match)){ # for each good match entry
  if(good.match[i] > 0){ # and we actually have a match
    if(!is.na(dvd.sales[good.match[1],2])){ # only if we have DVD sales - the target variable
      final.movie.match[df.row,] = cbind(merge1[i,merge.cols], dvd.sales[good.match[i],dvd.cols]) # insert the matched rows and column subsets
      #cat(good.match[i],' row ',merge1[i,1], ' and ',dvd.sales[good.match[i],1],'\n',sep='')
    }
    df.row <- df.row + 1     # increment df.row for the next one
  }
}
names(final.movie.match) <- c("title", "year", "rt.critics.rating", "rt.critics.score", "rt.audience.rating", "rt.audience.score", "studio", "bo.gross", "bo.screens", "bo.opening.gross", "bo.opening.screens", "open.date", "nyt.critics.pick", "nyt.1000.best", "nyt.sentiment.score", "first.week", "first.4.weeks", "first.8.weeks")

# remove records that are missing both score and rating - one cand be derived from the other, but not vice versa - 516 records
final.movie.match <- final.movie.match[(!is.na(final.movie.match$rt.critics.rating) | !is.na(final.movie.match$rt.critics.score)),]

#------------------
# now we need to do a little clean up here, replacing missing values
# this is not ideal, but it does get a workable data set
# in an ideal world, this data owuld be stored in a data warehouse, whose ETL processes enforced the necessary data quality standards
# R is not an ETL tool
#---------------------------

# need to set all 0 and -1 critic score to NA
final.movie.match[final.movie.match$rt.critics.score < 1, 4] <- NA
final.movie.match[final.movie.match$rt.audience.score < 1, 6] <- NA

# zero out the NA's for 1000 best and critics choice
final.movie.match[is.na(final.movie.match[,13]), 13] <- 0
final.movie.match[is.na(final.movie.match[,14]), 14] <- 0

# find all the complete cases
clean <- complete.cases(final.movie.match)
# predict critics score
train = final.movie.match[clean,]
critics.lm <- rpart(rt.critics.score ~ rt.audience.score + log(bo.gross) + bo.screens + log(bo.opening.gross) + bo.opening.screens + log(first.8.weeks) + nyt.sentiment.score, data = train)
# crit.train.pred <- predict(critics.lm, data=train )
# sqrt(sum((crit.train.pred - train$rt.critics.score)^2) / nrow(train)) # 17.47 - better than NA!
final.movie.match[is.na(final.movie.match$rt.critics.score),4] <- round(predict(critics.lm, final.movie.match[is.na(final.movie.match$rt.critics.score),]))
# predict audience
audience.lm <- rpart(rt.audience.score ~ rt.critics.score + log(bo.gross) + bo.screens + log(bo.opening.gross) + bo.opening.screens + log(first.8.weeks) + nyt.sentiment.score, data = train)
# aud.train.pred <- predict(audience.lm, data=train )
# sqrt(sum((aud.train.pred - train$rt.audience.score)^2) / nrow(train)) # 10.9 - better than NA!
final.movie.match[is.na(final.movie.match$rt.audience.score),6] <- round(predict(audience.lm, final.movie.match[is.na(final.movie.match$rt.audience.score),]))
# predict nyt sentiment
sentiment.lm <- rpart(nyt.sentiment.score ~ rt.critics.score + rt.audience.score + log(bo.gross) + bo.screens + log(bo.opening.gross) + bo.opening.screens + log(first.8.weeks), data = train)
# sent.train.pred <- predict(sentiment.lm, data=train )
# sqrt(sum((sent.train.pred - train$nyt.sentiment.score)^2) / nrow(train)) # 10.7 - better than NA!
final.movie.match[is.na(final.movie.match$nyt.sentiment.score),15] <- round(predict(sentiment.lm, final.movie.match[is.na(final.movie.match$nyt.sentiment.score),]))

# this only leaves the critics and audience ratings as NA, which are based on the score anyway
final.movie.match[(final.movie.match$rt.critics.score > 85 & is.na(final.movie.match$rt.critics.rating)),3] <- 'Certified Fresh'
final.movie.match[(final.movie.match$rt.critics.score > 59 & is.na(final.movie.match$rt.critics.rating)),3] <- 'Fresh'
final.movie.match[(is.na(final.movie.match$rt.critics.rating)),3] <- 'Rotten'
#final.movie.match[(final.movie.match$rt.audience.rating > 79 & is.na(final.movie.match$rt.audience.rating)),5] <- 'Upright'
final.movie.match[(is.na(final.movie.match$rt.audience.rating)),5] <- 'Fresh'


# write the file
write.csv(final.movie.match ,file="MovieData.csv", row.names=FALSE)