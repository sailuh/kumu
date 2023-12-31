% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrapping.R
\name{bootstrapping}
\alias{bootstrapping}
\title{Bootstrapping}
\usage{
bootstrapping(
  number_resampling,
  percent_resample_size,
  seed,
  add_original_dataset,
  resampling_with_replacement,
  resampling_ensemble,
  save_bootstrap_graphs
)
}
\arguments{
\item{number_resampling}{The number of bootstraps/resampling iterations (min = 0)}

\item{percent_resample_size}{The percentage of resample size (min = 10\%)}

\item{seed}{Seed for pseudorandom number generator (-1 = off)}

\item{add_original_dataset}{Yes, if adding the original dataset as another bootstrapping}

\item{resampling_with_replacement}{Yes, if sampling with replacement (bootstrapping)}

\item{resampling_ensemble}{Ensemble method: Preserved (1), Highest (2), Majority (3).
Use any other number to not include the flag.}

\item{save_bootstrap_graphs}{Yes if individual bootstrapping graphs should be saved}
}
\description{
Compose the flags of the bootstrapping
}
