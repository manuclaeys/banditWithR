% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LINUCB_rejection_sampling.R
\name{LINUCB_rejection_sampling}
\alias{LINUCB_rejection_sampling}
\title{LINUCB_rejection_sampling}
\usage{
LINUCB_rejection_sampling(
  dt,
  visitor_reward,
  alpha = 1,
  K = ncol(visitor_reward),
  IsRewardAreBoolean = FALSE
)
}
\arguments{
\item{dt}{Dataframe of integer or numeric values}

\item{visitor_reward}{Dataframe of integer or numeric values}

\item{alpha}{Numeric value (optional)}

\item{K}{Integer value (optional)}
}
\value{
\itemize{ List of element:
 \item choice: choices of UCB,
 \item proba: probability of the chosen arms,
 \item time: time of cumputation,
 \item theta_hat: mean estimated of each arm
 \item theta: real mean of each arm
 }
}
\description{
LINUCB algorithme with rejection sampling method.
Exclud any choices which not corresponds to real exepriments in dataset
Stop if something is wrong.
Generate a matrix to save the results (S).
\itemize{ At each iteration,
 \item Calculates the arm probabilities,
 \item Choose the arm with the maximum upper bound (with alpha parameter)
 \item Receives a reward in visitor_reward for the arm and associated iteration
 \item Updates the results matrix S.
 }
Returns the calculation time.
Review the estimated, actual averages and number of choices for each arm.
See also \code{\link{ConditionForUCB}}, \code{\link{GenerateMatrixS}},
\code{\link{ProbaMaxForUCB}} and \code{\link{PlayArm}}.
Require \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
}
\examples{
## Generates 10000 numbers from 2 binomial  distributions
set.seed(4434)
K1 <- rbinom(1000, 1, 0.6)
K2 <- rbinom(1000, 1, 0.7)
## Define a dataframe of rewards
visitor_reward <- as.data.frame(cbind(K1,K2) )
#remove data
temp_list <- sample(1:nrow(visitor_reward), 500, replace = FALSE, prob = NULL)
visitor_reward$K1[temp_list] <- NA
visitor_reward$K2[-temp_list] <- NA
#run ucb on missing data
ucb_rejection_sampling_alloc  <- UCB_rejection_sampling(visitor_reward,alpha = 10)
}
