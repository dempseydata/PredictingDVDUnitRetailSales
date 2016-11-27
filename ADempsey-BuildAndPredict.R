#================================================
# UCSC 2616 - final project - andrew dempsey
# 
# Train and test several regression models, in order to try and predict the retail sales volume of DVD units
# Note: Several columns are 'related' to each other, such as box office gross and box office for the opening weekend, which are also both related to the number of screens
# Questions:
#      1) What columns are most predictive?
#      2) Can the use of a few columns, be as effective as using them all?
#      3) Are the same columns equally effective at predicting the 3 different sales window measures?
# Algorithms to try:
#      Decision Tree (DT)
#      Random Forest (RF)
#      Linear Regression (LL)
#      Support Vector Machines (SVM)
#      Artifical Neural Networks (ANN)
#      Multivariate Additive Regression Splines (MARS)
# Code approach
# The approach used in this code is based upon that from "Data Mining with R - Learning with Case Studies" by Luis Torgo
# A big reason I have chosen this approach, is the framework in the DMwR package that allows for bulk build and test of 
# multiple formulas and algorithms, with cross validation, and the ability to select the best model from the collection and the parameters used.
# I find this pattern very easy to expand.
#================================================
# What variables do we have?
#  *title - title of the movie, for human recognition
#  *year - the year the movie was released at the movies
#   rt.critics.rating - A three level classification rating from the RottenTomatoes (RT) critics
#   rt.critics.score - numeric score from RT critics
#   rt.audience.rating - A three level classification rating from the RT forums
#   rt.audience.score - A numeric score from the RT forums
#  *studio - the studio who produced the movie
#   bo.gross - gross US box office recipts
#   bo.screens - total number of movie theaters the movie played in
#   bo.opening.gross - gross US box office receipts from the opening weekend
#   bo.opening.screens - total number of movie theaters the movie played in on the opening weekend
#   open.date - number of days since 1/1/1970 that the movie was released on (i.e. a date in continuous numerical form)
#   nyt.critics.pick - is the title a criritcs pick in the ney work times
#   nyt.1000.best - is the title on the NYT's best 1000 movies list - ALL ZEROS in my set, causes issues with some algorithms - EXCLUDE
#   nyt.sentiment.score - sum of naive count of positive and negative sentiment workds in the NYT critics review
# **first.week - first weeks DVD sales - target a
# **first.4.weeks - cumulative first 4 weeks DVD sales - target b
# **first.8.weeks - cumulative first 8 weeks DVD sales - target c
#   st.group - grouping of studios into those with15 or more moviesfrom 2009-2011 and thos with 12 or less
#
# * = exclude from model
# ** = target variables
#========================================================================================
#
# STEP 0: Set up my environment

my.start.all <- date()

# SET THE WORKING DIRECTORY TO WHAT IS APPROPRIATE FOR YOUR SYSTEM
setwd("/Users/adempsey/Documents/R/UCSC2612/FinalProject")

# read in required packages
require(plyr)
require(ggplot2)
require(reshape)
require(DMwR)  # from "Data Mining with R LEarning by Case Studies" book
require(stringr)

# required algorithms
require(rpart)  # decision tree
require(randomForest)  # random forest
require(e1071)  # svm's
require(nnet)  # neural networks
require(earth) # multiple attribute regression splines
require(MASS) # required for lm.ridge
require(parcor) # required for ridge.cv

#========================================================================================
#
# STEP 1: Defaine all of my 'learners'

# DT
my.dt <- function(form,train,test,...) {
  # Trims a decision tree based on the standard error, avoiding overfitting
  mod <- rpartXse(form,train,...)  # generate model ... allows passage of additional variables
  pred <- predict(mod,test)  # get the predictions
  mse <- mean((pred - resp(form,test))^2)  # get the mean squared error of the predictions
}

# RF
my.rf <- function(form, train, test, ...) {
  mod <- randomForest(form, data=train, ...) 
  pred <- predict(mod, test)
  mse <- mean((pred - resp(form, test))^2)
}

# LM
my.lm <- function(target, train, test, ...) {
  mod <- lm(target, data=train, ...)
  pred <- predict(mod, test)
  mse <- mean((pred - resp(target, test))^2) 
}

# SVM
# do I want to do this here? The tune approach seems much more flexible
# this function wont work with the experimentalcomparisons from dmwr and the built in cross validation
my.svm <- function(target, train, test, ...){
  # prams must be in the order of cost, gamma, degree, coef0
  mod <- svm(target, data=train)
  pred <- predict(mod, test)
  mse <- mean((pred - resp(target, test))^2)
  return(mse)
}

# these 4 functions just act as a 'label' and pass through all params to the underlying generic SVM function
my.linsvm <- function(target, train, test, ...){
  mse <- my.svm(target, train, test, ...)
}
my.polysvm <- function(target, train, test, ...){
  mse <- my.svm(target, train, test, ...)
}
my.rbfsvm <- function(target, train, test, ...){
  mse <- my.svm(target, train, test, ...)
}
my.sigsvm <- function(target, train, test, ...){
  mse <- my.svm(target, train, test, ...)
}

# ANN
my.nnet <- function(target, train, test, ...){
  mod <- nnet(target, train, ...)
  pred <- predict(mod, test)
  mse <- mean((pred - resp(target, test))^2)
  return(mse)
}
# linout=T - for linear outputs instead of logical
# maxit - maximum number of iterations
# size - number of units in hidden layer
# decay - weight decay
# entropy - entropy fitting, default is least-squares - ONLY for classification

# MARS
my.mars <- function(target, train, test, ...){
  mod <- earth(target, train, ...)
  pred <- predict(mod, test)
  mse <- mean((pred - resp(target, test))^2)
  return(mse)
}
# nk - max number of model terms before pruning
# degree - max degree of interaction
# thresh - forward stepping threshold, defines when it should terminate

#========================================================================================
#
# STEP 2: Get my data, create a scaled version and regular/scaled training and test sets

my.read.data <- date()

# read in the data
movie.data <- read.csv(file="MovieData-Tweaked.csv", stringsAsFactors=FALSE)

# pivot out the three categorical columns critics.ratings, audience.ratings, st.group
# bo.gross and bo.opening.gross are significantly bigger than all other numbers in the data, so taking the log value to compress the range is worthwhile to avoid it being overemphasized. Taking the log value also makes it easy to apply any models to new, unseen data, as the log only uses a single record, as opposed to the full dataset when normalizting the data to a mean of 0 and std.def of 1
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


# create training and test data sets - regular data
movie.sample <- sample(nrow(movie.data), size=100, replace=FALSE)
movie.train <- movie.data[-movie.sample,]
movie.test <- movie.data[movie.sample,]

#========================================================================================
#
# STEP 3: Build my test.groups in terms of formula + data set

my.formulas <- list(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

# build up my list of formula / datasets to use
# 1,2,3 = kitchen sink non-log for each of the three target variables - regular data
my.formulas[[1]] <- as.formula(first.week ~ rt.critics.score + rt.audience.score + bo.gross + bo.screens + bo.opening.gross + bo.opening.screens + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + aud.spilled + aud.fresh + aud.upright + little.studio + big.studio)

my.formulas[[2]] <- as.formula(first.4.weeks ~ rt.critics.score + rt.audience.score + bo.gross + bo.screens + bo.opening.gross + bo.opening.screens + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified+aud.spilled + aud.fresh+aud.upright + little.studio+big.studio)

my.formulas[[3]] <- as.formula (first.8.weeks ~ rt.critics.score + rt.audience.score + bo.gross + bo.screens + bo.opening.gross + bo.opening.screens + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified+aud.spilled + aud.fresh+aud.upright + little.studio+big.studio)

# 4,5,6 = early read non-log columns for each of the three target variables - regular data
my.formulas[[4]] <- as.formula(first.week ~ rt.critics.score + bo.opening.gross + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + little.studio + big.studio)

my.formulas[[5]] <- as.formula(first.4.weeks ~ rt.critics.score + bo.opening.gross + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + little.studio + big.studio) 

my.formulas[[6]] <- as.formula(first.8.weeks ~ rt.critics.score + bo.opening.gross + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + little.studio + big.studio)

# 7,8,9 = X most important non-log variables, for each of the three target variables - regular data
# this code performs random forest modeling and removes each variable to see how much the mean squared error increases (along with node purity). The higher in the increase in MSE when a variable is removed, the more important that variable is in the random forest and thus in predicting the target
set.seed(1234)
regular.importance.first.week <- randomForest(my.formulas[[1]], data=movie.train,importance=TRUE)$importance
set.seed(1234)
regular.importance.first.4.weeks <- randomForest(my.formulas[[2]], data=movie.train,importance=TRUE)$importance
set.seed(1234)
regular.importance.first.8.weeks <- randomForest(my.formulas[[3]], data=movie.train,importance=TRUE)$importance

# what are the top 5 most important attributes?
reg.order.first <- order(regular.importance.first.week[,1], decreasing=TRUE)[1:5]
my.formulas[[7]] <- as.formula(paste('first.week ~',paste(rownames(regular.importance.first.week)[reg.order.first], collapse=' + '), sep=''))

reg.order.first.4 <- order(regular.importance.first.4.weeks[,1], decreasing=TRUE)[1:5]
my.formulas[[8]] <- as.formula(paste('first.week ~',paste(rownames(regular.importance.first.4.weeks)[reg.order.first.4], collapse=' + '), sep=''))

reg.order.first.8 <- order(regular.importance.first.8.weeks[,1], decreasing=TRUE)[1:5]
my.formulas[[9]] <- as.formula(paste('first.week ~',paste(rownames(regular.importance.first.8.weeks)[reg.order.first.8], collapse=' + '), sep=''))

# 10,11,12 = kitchen sink log for each of the three target variables - scaled data
my.formulas[[10]] <- as.formula(first.week ~ rt.critics.score + rt.audience.score + log.bo.gross + log.bo.screens + log.bo.opening.gross + log.bo.opening.screens + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + aud.spilled + aud.fresh + aud.upright + little.studio + big.studio)

my.formulas[[11]] <- as.formula(first.4.weeks ~ rt.critics.score + rt.audience.score + log.bo.gross + log.bo.screens + log.bo.opening.gross + log.bo.opening.screens + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + aud.spilled + aud.fresh + aud.upright + little.studio + big.studio)

my.formulas[[12]] <- as.formula(first.8.weeks ~ rt.critics.score + rt.audience.score + log.bo.gross + log.bo.screens + log.bo.opening.gross + log.bo.opening.screens + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + aud.spilled + aud.fresh + aud.upright + little.studio + big.studio)

# 13,14,15 = early read log columns for each of the three target variables - scaled data
my.formulas[[13]] <- as.formula(first.week ~ rt.critics.score + log.bo.opening.gross + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + little.studio + big.studio) 

my.formulas[[14]] <- as.formula(first.4.weeks ~ rt.critics.score + log.bo.opening.gross + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + little.studio + big.studio)

my.formulas[[15]] <- as.formula(first.8.weeks ~ rt.critics.score + log.bo.opening.gross + open.date + nyt.critics.pick + nyt.sentiment.score + crit.rotten + crit.fresh + crit.certified + little.studio + big.studio)

# 16,17,18 = X most important log variables, for each of the three target variables - scaled data
#log-data
set.seed(1234)
log.importance.first.week <- randomForest(my.formulas[[10]], data=movie.train,importance=TRUE)$importance
set.seed(1234)
log.importance.first.4.weeks <- randomForest(my.formulas[[11]], data=movie.train,importance=TRUE)$importance
set.seed(1234)
log.importance.first.8.weeks <- randomForest(my.formulas[[12]], data=movie.train,importance=TRUE)$importance

# what are the top 5 most important attributes?
log.order.first <- order(log.importance.first.week[,1], decreasing=TRUE)[1:5]
my.formulas[[16]] <- as.formula(paste('first.week ~',paste(rownames(log.importance.first.week)[log.order.first], collapse=' + '), sep=''))

log.order.first.4 <- order(log.importance.first.4.weeks[,1], decreasing=TRUE)[1:5]
my.formulas[[17]] <- as.formula(paste('first.week ~',paste(rownames(log.importance.first.4.weeks)[log.order.first.4], collapse=' + '), sep=''))

log.order.first.8 <- order(log.importance.first.8.weeks[,1], decreasing=TRUE)[1:5]
my.formulas[[18]] <- as.formula(paste('first.week ~',paste(rownames(log.importance.first.8.weeks)[log.order.first.8], collapse=' + '), sep=''))


# build up three formulas to predict each target variable, with all numerical columns
test.groups <- (list(
  # 1-3 kitchen sink non-log
  dataset(my.formulas[[1]], movie.train), 
   dataset(my.formulas[[2]], movie.train), 
   dataset(my.formulas[[3]], movie.train), 
  # 4-6 early read non log
  dataset(my.formulas[[4]], movie.train), 
   dataset(my.formulas[[5]], movie.train), 
   dataset(my.formulas[[6]], movie.train),
  # 7-9 Most important columns non log
   dataset(my.formulas[[7]], movie.train), 
   dataset(my.formulas[[8]], movie.train), 
   dataset(my.formulas[[9]], movie.train),
  # 10-12 kitchen sink log
  dataset(my.formulas[[10]], movie.train), 
   dataset(my.formulas[[11]], movie.train), 
   dataset(my.formulas[[12]], movie.train),
  # 13-15 early read log
  dataset(my.formulas[[13]], movie.train), 
   dataset(my.formulas[[14]], movie.train), 
   dataset(my.formulas[[15]], movie.train),
  # 16-18 Most important columns log
   dataset(my.formulas[[16]], movie.train), 
   dataset(my.formulas[[17]], movie.train), 
   dataset(my.formulas[[18]], movie.train)))


#========================================================================================
#
# STEP 4: Run a quick couple of initial tunings, to find some starting 'best values' for various SVM's
# using the approach below and testing many different combinations of SVM paramters, it was discovered that the best
# paramters for non-linear SVM's was ZERO - i.e. the problem is a linear regression one
# However, sigmoid did perform well on the last of the 6 test sets - 'best 6' columns and log data
# Therefore, only linear and sigmoid SVM's shall be included in the final testing
# Tests were only performed against the first.week target only - 6 formulas
# res.all <- experimentalComparison(
#   test.groups,
#   c(
#     # Inital testing of linear (51 variations - 30 mins) gave best performance at cost = 0
#     variants('my.linsvm',kernel='linear', cost=seq(0,10,0.2)),
#     # Inital testing of polynomial (4410 variations - 379 mins), gave best performance at cost=0, gamma=0, degree=1, coef0=0
#     variants('my.polysvm',kernel='polynomial', cost=seq(0,10,0.5), gamma=seq(0,10,0.5), degree=(1,2), coef0=seq(0,2,0.5)),
#     # Inital testing of radial (441 variations - 33 mins), gave best performance at cost=0, gamma=0
#     variants('my.rbfsvm',kernel='radial', cost=seq(0,10,0.5), gamma=seq(0,10,0.5)),
#     # Inital testing of sigmoid (4410 variations- 412 mins), gave best performance at cost=0, gamma=0, degree=1, coef0=0
#     variants('my.sigsvm',kernel='sigmoid', cost=seq(0,10,0.5), gamma=seq(0,10,0.5), degree=c(1,2), coef0=seq(0,2,0.5)),
#     # Initial testing of ANN (300 variations = 138 mins), give best values maxit=(100,200,300,400,600), decay=(0.01, 0.1), size=(2,8,10,16,18)
#     variants('my.nnet', linout=TRUE, maxit=seq(100,1000,100), size=seq(2,20,2), decay=c(0.001,0.01,0.1), entropy=FALSE)#, # 300 ANN - 138 mins
#     ),
#   cvSettings(1,10,1234))
#
#  Looking at ridge regression - best lambda is 1 (linear) so will not include in the bulk testing
# lambdaSet <- c()
# # create a set of exponentially spaced lambdas from 1000 to... almost 0 - review the formula
# for (iLambda in seq(from = 0, to = 20)) {
#   exp <- +3 - 4*(iLambda/20)
#   lambdaSet <- c(lambdaSet, 10^exp)
# }  
# 
# # create objects for the required columns in the formula and target
# # formulas cannot be used with ridge.cv
# trainX <- movie.train[,c(5,7,8,9,10,11,12,13,15,20,21,22,23,24,25,26,27)]
# # nyt.1000.best is scalled to NaN - exclude from test
# trainY <- movie.train[,16]
# #Use a 10-fold cross validation along various values of lambda
# ridge.test <- ridge.cv(as.matrix(trainX), trainY, scale=TRUE, lambda = lambdaSet, k = 10, plot.it = FALSE)
# ridge.test$lambda.opt


#========================================================================================
#
# STEP 5: Run my test.groups through each of my learners, with multiple parameters for each learner


# this function runs through all the defined data sets, and applies all of the models and their paramter combinations, to capture the performance of each
# 168 models, 18 formulas = 3024 models = 94 mins
my.start.model <- date()
res.all <- experimentalComparison(
  test.groups,
   c(variants('my.dt', se=c(0,0.25,0.5,1)),  # 4 DT
     variants('my.rf', ntree=c(100,250,500,750,1000), maxnodes=c(5,10,15,20,25,30)),  # 30 RF
     variants('my.lm'),  # 1 LM - returns "prediction from a rank-deficient fit may be misleading" errors when using the 'best columns' formulas with 6 columns?
     variants('my.linsvm',kernel='linear', cost=seq(0,1,0.05)), # 20 lin-SVM
     variants('my.sigsvm',kernel='sigmoid', cost=seq(0,0.2,0.05), gamma=seq(0,0.2,0.05), degree=1, coef0=seq(0,0.5,0.5)), # 50 sig-SVM
     variants('my.nnet', linout=TRUE, maxit=c(100,200,300,400,600), size=c(2,8,10,16,18), decay=c(0.01,0.1), entropy=FALSE), # 50 ANN
     variants('my.mars', nk=c(3,4,5), degree=c(1,2), thresh=c(0.0001,0.001,0.01)) # 24 mars - keep running into subscript out of bounds and other errors when using the 'best columns' formulas with 6 columns?
    ),
  cvSettings(1,10,1234))
my.end.model <- date()
my.model.runtime <- difftime(strptime(my.end.model, "%a %b %d %H:%M:%S %Y"),strptime(my.start.model, "%a %b %d %H:%M:%S %Y"),units="mins")
print(my.model.runtime)

#========================================================================================
#
# STEP 6: get the top N models for each test group, rebuild them and evaluate performance with the test data

my.best.start <- date()

# get the best performing model for each data set
# model names are in the format of <function used>.<version number> such as my.rf.v32
best.builds <- sapply(bestScores(res.all),function(x) x['system'])

# this gives me the top X from each set
#rankSystems(res.all, top = 5) # might be better to use this, esp if the performance is 'close', perhaps taking an average of them for the final

# translate a portion of the my.function name, into the actual function name that will be used to build the model for testing
learners <- c(rf='randomForest',dt='rpartXse', lm='lm', linsvm='svm', polysvm='svm', rbfsvm='svm', sigsvm='svm', nnet='nnet', mars='earth')

# get the middle portion of the best models names, and translate them into the actual model build function that will be used to build and test the final model
funcs.used <- learners[  # get from the list of my.function to real.function translations - 'randomForest'
  unlist(  # need to unlist from a list object - 'rf'
    lapply(  # apply the following to eachmember of the list
      best.builds,  # list of best models like my.rf.v32
      function(x) str_sub(x[1], # from this string
                          1 + str_locate_all(x[1], '\\.')[[1]][1], # from the first position of a period, plus 1 to skip the period
                          str_locate_all(x[1], '\\.')[[1]][2] - 1)))] # up until second position of a period, minus 2 to skip the period 

# gets the parameters of the functions used in each of the best models
params.used <- lapply(best.builds,function(x) getVariant(x,res.all)@pars)

# concatenate the best model build call together and apply the test data, to see how they perform
# create a list object for the models to go into
best.models <- list()
# a list for my own text reference
best.models.details <- list()

# basically parses together an R function call that REBUILDS the model that was created in the earlier code
#for(a in 1:length(test.groups)) { # there is a scalingissue with formulas 16,17,18, due to 'little.studio' - not sure why, wince they did not fail above
  for(a in 1:15) {
  # e.g. rpartXse('a1 ~.', etc....) - concatenated string from the variables
  set.seed(1234)
  best.models[[a]] <- do.call(funcs.used[[a]], c(list(my.formulas[[a]], movie.train), params.used[[a]]))
  best.models.details[[a]] <- c(a, funcs.used[[a]], params.used[[a]])
}

my.best.end <- date()

#========================================================================================
#
# STEP 7: Which model is best for each target variable

my.testing.start <- date()
# predictions matrix, for all data sets
test.preds <- matrix(ncol=length(best.models),nrow=100)

# for each row in the test data
for(i in 1:nrow(movie.test))
  # apply function for 1 to 7, of each X - being the second coordinate in preds[]
  test.preds[i,] <- sapply(1:15, function(x) predict(best.models[[x]],movie.test[i,]))


# get the MSE of the predictions versus the solutions
test.preds.mse <- matrix(NA,nrow=1, ncol=length(best.models))
test.preds.mse[1] <- sum((movie.test[,16] - test.preds[,1])^2)/nrow(movie.test)
test.preds.mse[2] <- sum((movie.test[,17] - test.preds[,2])^2)/nrow(movie.test)
test.preds.mse[3] <- sum((movie.test[,18] - test.preds[,3])^2)/nrow(movie.test)
test.preds.mse[4] <- sum((movie.test[,16] - test.preds[,4])^2)/nrow(movie.test)
test.preds.mse[5] <- sum((movie.test[,17] - test.preds[,5])^2)/nrow(movie.test)
test.preds.mse[6] <- sum((movie.test[,18] - test.preds[,6])^2)/nrow(movie.test)
test.preds.mse[7] <- sum((movie.test[,16] - test.preds[,7])^2)/nrow(movie.test)
test.preds.mse[8] <- sum((movie.test[,17] - test.preds[,8])^2)/nrow(movie.test)
test.preds.mse[9] <- sum((movie.test[,18] - test.preds[,9])^2)/nrow(movie.test)
test.preds.mse[10] <- sum((movie.test[,16] - test.preds[,10])^2)/nrow(movie.test)
test.preds.mse[11] <- sum((movie.test[,17] - test.preds[,11])^2)/nrow(movie.test)
test.preds.mse[12] <- sum((movie.test[,18] - test.preds[,12])^2)/nrow(movie.test)
test.preds.mse[13] <- sum((movie.test[,16] - test.preds[,13])^2)/nrow(movie.test)
test.preds.mse[14] <- sum((movie.test[,17] - test.preds[,14])^2)/nrow(movie.test)
test.preds.mse[15] <- sum((movie.test[,18] - test.preds[,15])^2)/nrow(movie.test)

# Which model was used - need to improve upon reporting WHICH model perforemd best - create another variable like 'best models' but without the data
best.models.details[[which(test.preds.mse == min(test.preds.mse))[1]]]
my.formulas[best.models.details[[which(test.preds.mse == min(test.preds.mse))[1]]][[1]]]
test.preds.mse[best.models.details[[which(test.preds.mse == min(test.preds.mse))[1]]][[1]]]

# create new DF actual and predicted values, write to file, for the best model only - first.8.weeks target
final.results <- data.frame(actual = movie.test[,18], predicted = test.preds[,best.models.details[[which(test.preds.mse == min(test.preds.mse))[1]]][[1]]])

write.csv(final.results,'finalpredictions.csv', row.names=FALSE)

# quick plot of results
final.results.melted <- melt(final.results)
ggplot(final.results.melted) + 
  geom_density(aes(x = value, colour = variable)) + 
  labs(x = NULL) +
  opts(title = "Actual vs Predicted DVD Sales (Scaled)")

ggsave('finalpredictions.png', width=6,height=6)


my.test.end <- date()