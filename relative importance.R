setwd("/Users/adempsey/Documents/R/UCSC2612/FinalProject")
require(plyr)
require(reshape2)

movie.data <- read.csv(file="MovieData-Tweaked.csv", stringsAsFactors=FALSE)

movie.data <- mutate(movie.data,
                     crit.rotten = ifelse(movie.data$rt.critics.rating == 'Rotten',1,0),
                     crit.fresh = ifelse(movie.data$rt.critics.rating == 'Fresh',1,0),
                     crit.certified = ifelse(movie.data$rt.critics.rating == 'Certified Fresh',1,0),
                     aud.spilled = ifelse(movie.data$rt.audience.rating == 'Spilled',1,0),
                     aud.fresh = ifelse(movie.data$rt.audience.rating == 'Fresh',1,0),
                     aud.upright = ifelse(movie.data$rt.audience.rating == 'Upright',1,0),
                     little.studio = ifelse(movie.data$st.group == 'Little Studio',1,0),
                     big.studio = ifelse(movie.data$st.group == 'Big Studio',1,0),
                     log.bo.gross = log(bo.gross),
                     log.bo.screens = log(bo.screens),
                     log.bo.opening.gross = log(bo.opening.gross),
                     log.bo.opening.screens = log(bo.opening.screens))

#numeric columns
num.cols <- c(3,5,7:18,20:31)


# this is a great diagram showing the correlation between all numerical variables in the data set. A potential way of reducing the dataset by REMOVING overly correlated data and keeping the more unrelated items
require(PerformanceAnalytics)
chart.Correlation(movie.data[,num.cols])

# this plot is another, more concise way of plotting the correlations between different columns. Again showing areas of strong correlation
require(psych)
cor.plot(cor(movie.data[,num.cols]))

# but what about relative importance
# lets do a quick regressions analysis (using all columns except 3 targets of first/4/8 week)

scaled.movie.data<-data.frame(scale(movie.data[,num.cols]))
## some result in NA use in the model, so lets remove those columns
## also, lets remove gross box office info
movie.model <- lm(first.week ~ rt.critics.score + rt.audience.score + + bo.opening.gross + bo.opening.screens + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + aud.spilled + aud.fresh + little.studio, 
                  data=scaled.movie.data)

# now that we have a model, lets look at the details
summary(movie.model)

# and what their importance is
require(relaimpo)
movie.impo <- calc.relimp(movie.model, type = c("lmg"), rela = TRUE)
# a nice summary along with % controbution is given, but I dont know how to access a subset of the object programattically

# now lest have a look at a network diagram
require(qgraph)
# just looking at a subset of the columns, we can have a look at a network diagram
# of how the various columns are 'related' in terms of moving together
# green links are strength of positive correlation
# red links are strength of negative correlation
# weaker links are NOT drawn if they are below the minimum)
num.cols <- c(5,7,10,11,15,16,20:25)
gr<-list(1:12) 
qgraph(cor(movie.data[,num.cols]),layout="spring", groups=gr, labels=names(movie.data[,num.cols]), label.scale=FALSE, minimum=0.10)