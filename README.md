# IMDB-Database-Exploration

Interenet Movie Database (IMDb) is the world's most popular and authoritative source for online information related to films, television series, home videos, video games, and streaming content online. The purpose of this app is to explore the IMDb datasets and the relation between different variables in the datasets. In this regard, title basics and title ratings, two datasets from IMDb datasets has been used in this app. From title basics dataset titleID, titleType, primaryTitle, isAdult, startYear, runtimeMinutes, genre and from title ratings dataset averageRating and numVotes variables have been used in this app. Following is the link for datasets from IMDb dataset website.
https://www.imdb.com/interfaces/
Using this app users can perfrom descriptive statistics, visualization and statistical analysis of variables of IMDb datasets: title basics and title ratings. 

## How to start the software

#### Requirement
* R >= 4.0.2
* Shiny >= 1.2.0

* Open R 
```R
install.packages('shiny')
install.packages('shinythemes')
library(shiny)
library(shinythemes)
shiny::runGitHub("IMDB-Database-Exploration", "tasnimmajumder", ref="main")
```

## Tutorial of IMDB-Database-Exploration can be downloaded at


## Run IMDB-Database-Explortaion using shinyapps.io with free RStudio service

https://tasnimmajumder-1993.shinyapps.io/IMDb_explorer/

## Brief Instruction

In the first tab of this app users can perfrom descriptive statistics for the variables. For Continuous variables, Start Year (startYear), Average Rating (averageRating), Runtime in Minutes (runtimeMinutes) and Number of Votes (numVotes), following functions are provided: mean, median, standard deviation, maximum and minimum. For categorical variables, Title Type (titleType), Genre (genre) and Is Adult (isAdult), summary table function has been provided.

In the second tab, visualization, both for continuous and categorical variables, ggplot has been used to show the relations among the variables. For two continuous variables and one catefgorical variable geom_smooth and geom_point have beeen used. On the other hand, for two categorical variables and one continuous variable geom_boxplot has been used.

In the third tab, statistical analysis, correlation test and anova test have been provided to do hypothesis testing for all variables. Correlation test has been provided to find the correlation value and statistical significance of correlation value between two continuous variables. On the other hand, anova test has provided for one continuous and one categorical variable to find difference in mean of categorical variable's categories for the continuous variable.

