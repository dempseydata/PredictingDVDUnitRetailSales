#================================================
# Process the NYTimes review data, to extract the niave sentimwnt in terms of overall
# number of positive and negative words. A positive score means there were more positive
# words than negative words, a negative score means the opposite.
# Score ranges from -44 to 40
#
# Using code based upon
#  http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment
#================================================

# SET THE WORKING DIRECTORY TO WHAT IS APPROPRIATE FOR YOUR SYSTEM
setwd("/Users/adempsey/Documents/R/UCSC2612/FinalProject")

# Load the required packages
require(plyr)
require(stringr)

# get the movie data
movies <- read.csv("NYTimes-Final.csv", stringsAsFactors=FALSE)
# get the lexicons for positive and negative words
pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

# To score each tweet, our score.sentiment() function uses laply() to iterate through the input text. It strips punctuation and control characters from each line using R’s regular expression-powered substitution function, gsub(), and uses match() against each word list to find matches
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{

  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# score the reviews
results <- score.sentiment(movies[,6], pos.words,neg.words, .progress="text")
# the .progress gives on screen feedback as to how far trough the data frame we are - it is part of the PLYR package

# add the scores to the NYTimes data, and drop the review text and URL (want cols 1:4)
movie.sentiment <- cbind(movies[,1:4], results$score)
names(movie.sentiment) <- c("display.title","critics.pick","thousand.best","opening.date","sentiment.score")

# write the file
write.csv(movie.sentiment ,file="NYTimes-Sentiment.csv", row.names=FALSE)

#max(movie.sentiment$sentiment.score)
#min(movie.sentiment$sentiment.score)