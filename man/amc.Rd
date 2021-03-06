% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amc.R
\name{amc}
\alias{amc}
\title{Adaptive Mixture Categorization}
\usage{
amc(data)
}
\arguments{
\item{data}{The input data for categorization (should be a data frame)}
}
\value{
The categorized data, the number of categories, and the categorization thresholds.
\item{output}{The categorized data}
\item{cutoff}{The thresholds/cutoffs of the categorization for each variable}
\item{group}{The number of categories for each variable}
}
\description{
This is the function for the Adaptive Mixture Categorization, which can categorize variables in a flexible way using F-statistic.
}
