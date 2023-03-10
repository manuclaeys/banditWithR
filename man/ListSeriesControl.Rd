% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ListSeriesControl.R
\name{ListSeriesControl}
\alias{ListSeriesControl}
\title{ListSeriesControl}
\usage{
ListSeriesControl(listSeriesList, dt)
}
\arguments{
\item{listSeriesList}{of characters vectors}

\item{dt}{Dataframe of integer numeric or factor values}
}
\value{
Logical value?c
}
\description{
Control if the ListSeries elements are in on the dataframe colomn names
Print a message and stop if this condition is not respected.
Else return TRUE.
}
\examples{
## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rnorm(100, 30, .05)
K2 <- rnorm(100, 35, .05)
## Define a dataframe of rewards
dt <- as.data.frame(cbind(K1,K2))
## Define a dataframe of context
l <- c("K1","K2")
ListSeriesControl(listSeriesList = l,dt=dt)

}
