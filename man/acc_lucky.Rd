% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric.R
\name{acc_lucky}
\alias{acc_lucky}
\title{A function to check whether a classification is better than a guess}
\usage{
acc_lucky(train_class, test_class, my_acc, s = 1000)
}
\arguments{
\item{train_class}{a vector for the distribution of classes in the training set}

\item{test_class}{a vector for the distribution of classes in the test set}

\item{my_acc}{a number between 0 and 1 for the classification accuracy to be evaluated}

\item{s}{sample size of simulations used to check p-values}
}
\description{
A function to check whether a classification is better than a guess
}
\examples{
train_class <- c(1223,1322,1144)
test_class <- c(345,544,233)
my_acc <- 0.45
acc_lucky(train_class, test_class, my_acc)

}
\author{
Larry Lei Hua
}
