% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thompson_sampling.R
\name{ThompsonSampling}
\alias{ThompsonSampling}
\title{ThompsonSampling}
\usage{
ThompsonSampling(visitor_reward, K = ncol(visitor_reward), alpha = 1, beta = 1)
}
\arguments{
\item{visitor_reward}{Dataframe of integer or numeric values}

\item{K}{Integer value (optional)}

\item{alpha}{Numeric value (optional)}

\item{beta}{Numeric value (optional)}
}
\value{
\itemize{ List of element:
 \item S:numerical matrix of results ,
 \item choice: choices of TS,
 \item proba: probability of the chosen arms,
 \item time: time of cumputation,
 \item theta_hat: mean estimated of each arm
 \item theta: real mean of each arm
 }
}
\description{
A thompson sampling (TS) bandit strategy implemented by sampling, in each round, averages from a posterior
distribution  \code{\link{ConditionForThompsonSampling}}, and choosing the action that maximizes the expected reward given the
sampled average. Conceptually, this means that the player instantiates their beliefs
randomly in each round, and then acts optimally according to them.
Control data in visitor_reward with \code{\link{IsRewardAreBoolean}}
Stop if something is wrong.
Generate a matrix to save the results (S).
\itemize{ At each iteration
 \item Sample an averages from a posterior in S for each arm (beta distribution with alpha and beta parameters)
 \item Choose the arm with the highest average
 \item Receives a reward in visitor_reward for the arm and associated iteration
 \item Updates the results matrix S.
 }
Returns the calculation time.
Review the estimated, actual averages and number of choices for each arm.
See also  \code{\link{ConditionForThompsonSampling}}, \code{\link{GenerateMatrixS}}, and \code{\link{PlayArm}}.
Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
}
\examples{
## Generates 1000 numbers from 2 uniform distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitor_reward <- as.data.frame( cbind(K1,K2) )
ThompsonSampling(visitor_reward)
}
