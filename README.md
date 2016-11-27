# PredictingDVDUnitRetailSales

---

_This project was written in Fall 2012 and used R 2.14, and used as part of a machine learning class. Along with the issues and improvements noted in the report and in the readme, the project would have benefited greatly from using the [caret package[ :ftp://cran.r-project.org/pub/R/web/packages/caret/caret.pdf , benefiting from a more consistent set of model functions and crossfold validation._

---

Final project for Uni. California Santa Cruz, Silicon Valley Extension, 2612, Introduction to machine learning.

Predict DVD unit retail sales using a combination of US and worldwide box office receipts along media and social ratings (rotten tomatoes, New York Times). Specifically, the ratings will be those of experts and the crowd, along with simplistic sentiment analysis of the reviews. All of the data inputs, are available prior to a movie being released for retail sale.

I used combinations of input columns to model and predict each of the three target columns. In each case, only the numerical input columns are considered.

1. All input columns
2. Early read columns
 * Columns such as those from the New York Times and opening weekend box office receipts, being those columns available before others such as audience ratings. An early read, might give competitive benefits in terms of purchasing the desired volume before supply becomes limited
3. ‘Best 5 columns’
 * The best 5 columns were determined for each target column, by using a random forest function that looks at the increase in mean squared error when each input column is removed in turn

These three combinations of columns, were done with either the raw box office numbers or the log box office numbers, not both – in other words, 6 combinations of columns were used to predict 3 different target columns - A total of 18 different formulas.

Each of these formulas was then used with 6 different regression models, to give a total of 108 models, not counting for any tuning parameter combinations.

1. Decision trees
2. Random forest
3. Linear regression
4. Support vector machines
5. Neural networks
6. Multiple attribute regression splines

When adding in the different combinations of parameters to these algorithms, a total of 3024 models were built and tested. The process of all these models builds was aided significantly by the use of the framework demonstrated in the book  [“Data Mining with R – Learning with Case Studies”]: http://www.dcc.fc.up.pt/~ltorgo/DataMiningWithR/ by Luis Torgo.

## Issues and Improvements

Whilst the overall concept has been proved, that a mix of popularity data can be used to predict DVD retail unit sales, the overall accuracy of this first attempt would be unacceptable for a real world implementation. The following is a list of ways in which performance of predictions should be able to be improved:

1.	Better data quality through the use of an appropriate ETL tool
  *	Better scraping functionality
  *	More robust file merging
2.	Better handling of missing values
  *	Perhaps the use of K nearest neighbors instead of a linear regressions
3. 	Additional data sources and columns
  *	DVD format – but this would mean even more target columns
  *	Genre – Different genres attract different audiences, but also sell different formats to different extents (e.g. ‘The Help” likely will not sell Blu ray and 3D in quite the same proportions as “Avatar” will)
  *	Series – To a certain extent, subsequent ‘Harry Potter’ DVD’s, benefit from the reputation of previous ‘Harry Potter’ films
  *	‘Successful Director’ and ‘Popular Hollywood Star’ (as of the date of theatrical release and DVD street date) flags 
  *	Better sentiment analysis that takes into account the context of the usage of any particular word.
  *	Additional sentiment sources with a broader range of opinions, rather than a single source

In addition to the above data improvements, there is a potential that the code framework that was used could be parallelized easily using R 2.14 and the ‘parallel’ package, in order to reduce the execution time. However, the issue with this is in the use of an ‘S4’ object (the list of lists of formula plus data, I believe) which is not easily vectorized for parallel execution.
