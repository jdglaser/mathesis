## Using Machine Learning and Google Searches to Predict Stock Market Changes

### MA Thesis - Jarred Glaser
#### Spring 2018

This repo holds the code for my MA thesis: "Using Machine Learning and Google Searches to Predict Stock Market Changes".

### MAThesisR.R

The main script that I used to generate features, run random forest and SVM models, and plot data.

__Packages Used in this script:__
+ ggplot2
+ tidyverse
+ fpp2
+ TTR
+ pracma
+ data.tables
+ caret
+ mlbench
+ randomForest
+ gridExtra
+ ggpubr
+ mlbench
+ e1071

### GoogleSearchVolume.py

A Python script I created and used to collect and normalize daily Google search data. Special thanks and credit goes to a module called `pytrends`, a pseudo API created by [GeneralMills](https://github.com/GeneralMills) for grabbing data from Google Trends. See the module [here](https://github.com/GeneralMills/pytrends).

__Modules used in this script:__

+ pytrends
+ datetime
+ pandas
+ matplotlib
+ time
+ random
