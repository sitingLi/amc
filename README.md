# Adaptive-mixture-categorization (AMC)-based g-computation
Adaptive-mixture-categorization (AMC)-based g-computation approach combines g-computation with an optimal categorization search using the F statistic. It reduces variance within each category and retains the variance between categories to build more powerful predictors for mixture analysis. 

## Install
The R package 'amc' can be installed from Github using the command:

``` r
# install.packages("devtools")
devtools::install_github("sitingLi/amc")
```

## Main function

``` r
amc(data)
```
This function is for categorizing variables into the optimal number of categories. For the input data, the number of categories for different variables (columns) can be different.

**Input**
- data: the input data is in the form of a data frame

**Output**
- output: the categorized data
- group: the number of categories for each variable
- cutoff: the thresholds/cutoffs of the categorization for each variable

``` r
amc.fixed.k(data, group, min_num)
```
This function is for categorizing variables into specified number of categories (k). The number of categories for different variables (columns) is the same.

**Input**
- data: the input data for categorization
- group: the number of categories, which can be set as an integer from 2 to 10 by the user. (default = 2) We recommend the number is less than 6.
- min_num: the minimum sample size of each category, which can be set as a positive integer. (default = 1) We recommend the number is larger than 5 percent of the overall sample size.

**Output**
- output: the categorized data
- cutoff: the thresholds/cutoffs of the categorization for each variable

