% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_cum_regret_for_each_subgroupe.R
\name{plot_cum_regret_for_each_subgroupe}
\alias{plot_cum_regret_for_each_subgroupe}
\title{plot_cum_regret_for_each_subgroupe}
\usage{
plot_cum_regret_for_each_subgroupe(ctree_ucb_object, average = FALSE)
}
\arguments{
\item{ctree_ucb_object}{Dataframe of integer or numeric values}
}
\description{
plot cum regret for each subgroupe of ctreeucb object. See \code{\link{ctree}} using visitor_reward and dt values.
}
\examples{
size.tot = 1000
set.seed(4649)                          # this makes the example exactly reproducible
x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
x2 = runif(size.tot, min=0, max=10)
x3 = runif(size.tot, min=0, max=10)
x4 = runif(size.tot, min=0, max=10)
dt = cbind(x1,x2,x3,x4)
arm_1 <-  as.vector(c(-1,9,-8,4))
K1 = crossprod(t(dt),arm_1)
arm_2 <-  as.vector(c(-1,2,1,0))
K2 = crossprod(t(dt),arm_2)
arm_3 <-  as.vector(c(-1,-5,1,10))
K3 = crossprod(t(dt),arm_3)
visitor_reward <-  data.frame(K1,K2,K3)
dt <- as.data.frame(dt)
temp <- ctreeucb(dt,visitor_reward)
plot_cum_regret_for_each_subgroupe(temp)
}
