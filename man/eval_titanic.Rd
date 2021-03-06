% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/titanic.R
\name{eval_titanic}
\alias{eval_titanic}
\title{Evaluates accuracy and logloss for the Titanic test set.}
\usage{
eval_titanic(pred)
}
\arguments{
\item{pred}{a vector with 272 numeric values representing survival probabilities for all passengers in the Titanic
test data.}
}
\value{
a list with 2 attributes: accuracy and logloss.
}
\description{
This function evaluates the accuracy and logloss of predictions for the Titanic test set (272 individuals).
}
\examples{
library(isd)
pred <- runif(272)
eval_titanic(pred)
}
\author{
Olivier François
}
