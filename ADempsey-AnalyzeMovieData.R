#================================================
# UCSC 2616 - final project - andrew dempsey
# Analyze the movie data
#
# 
#================================================

# SET THE WORKING DIRECTORY TO WHAT IS APPROPRIATE FOR YOUR SYSTEM
setwd("/Users/adempsey/Documents/R/UCSC2612/FinalProject")

# read in required packages
require(plyr)
require(ggplot2)
require(DMwR)  # from "Data Mining with R LEarning by Case Studies" book

# read in the data
movie.data <- read.csv(file="MovieData.csv", stringsAsFactors=FALSE)

#What variables do we have?
# title - title of the movie, for human recognition
# year - the year the movie was released at the movies
# rt.critics.rating - A three level classification rating from the RottenTomatoes (RT) critics
# rt.critics.score - numeric score from RT critics
# rt.audience.rating - A three level classification rating from the RT forums
# rt.audience.score - A numeric score from the RT forums
# studio - the studio who produced the movie
# bo.gross - gross US box office recipts
# bo.screens - total number of movie theaters the movie played in
# bo.opening.gross - gross US box office receipts from the opening weekend
# bo.opening.screens - total number of movie theaters the movie played in on the opening weekend
# open.date - number of days since 1/1/1970 that the movie was released on (i.e. a date in continuous numerical form)
# nyt.critics.pick - is the title a criritcs pick in the ney work times
# nyt.1000.best - is the title on the NYT's best 1000 movies list
# nyt.sentiment.score - sum of naive count of positive and negative sentiment workds in the NYT critics review
# first.week - first weeks DVD sales - target a
# first.4.weeks - first 4 weeks DVD sales - target b
# first.8.weeks - first 8 weeks DVD sales - target c

# all of these charts seem to indicate a correlation between log(bo.gross), log(bo.opening.gross) and the fiirst.week sales
# they also seem to indicate
qplot(log(bo.gross),first.week,color=studio, data=movie.data, geom='point') + facet_grid(rt.audience.rating~rt.critics.rating)
qplot(log(bo.opening.gross),first.week,color=studio, data=movie.data, geom='point') + facet_grid(rt.audience.rating~rt.critics.rating)
# there might even bve some variability by studio, HOWEVER, I need to collapse the studio variables down into something like 'big N plus others", based upon a threshold for the nmber of movies in this data set
qplot(log(bo.opening.gross),first.week,color=rt.critics.rating, shape=rt.audience.rating, data=movie.data, geom='point') + facet_wrap(~studio)

# 71 studios have less than 11 films
# 14 have 15 or more films
# so lets add a studio.grouping
studio.grouping.ref <- mutate(arrange(ddply(movie.data,.variables='studio',.fun=summarize, num.films = length(unique(title))), desc(num.films)), st.group =ifelse(num.films >14,'Big Studio', 'Little Studio'), stringsAsFactors=FALSE)[,c(1,3)]

# why does using JOIN from plyr, give me an error? Merge with the same params, works fine
movie.data <- merge(movie.data, studio.grouping.ref, by = "studio", type='outer')

qplot(log(bo.opening.gross),first.week,color=rt.audience.rating, data=movie.data, geom='point') + facet_grid(st.group~rt.critics.rating)

# so, initial conclusion: There is some predictive ability in this dataset, for some of the columns, but not all. This is seen using eith bo.gross or bo.opening.gross, the latter being better as it gives more LEAD time in what would be an invgentory purchasing decision


write.csv(movie.data ,file="MovieData-Tweaked.csv", row.names=FALSE)