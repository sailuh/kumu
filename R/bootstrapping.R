# Kumu - https://github.com/sailuh/kumu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Bootstrapping
#'
#' Compose the flags of the bootstrapping
#'
#' @param number_resampling The number of bootstraps/resampling iterations (min = 0)
#' @param percent_resample_size The percentage of resample size (min = 10\%)
#' @param seed Seed for pseudorandom number generator (-1 = off)
#' @param add_original_dataset Yes, if adding the original dataset as another bootstrapping
#' @param resampling_with_replacement Yes, if sampling with replacement (bootstrapping)
#' @param resampling_ensemble Ensemble method: Preserved (1), Highest (2), Majority (3).
#' Use any other number to not include the flag.
#' @param save_bootstrap_graphs Yes if individual bootstrapping graphs should be saved
#' @export
bootstrapping <- function(number_resampling,
                          percent_resample_size,
                          seed,
                          add_original_dataset,
                          resampling_with_replacement,
                          resampling_ensemble,
                          save_bootstrap_graphs){

  if(add_original_dataset){
    add_original_dataset <- "--addOriginalDataset"
  }else{
    add_original_dataset <- ""
  }
  if(resampling_with_replacement){
    resampling_with_replacement <- "--resamplingWithReplacement"
  }else{
    resampling_with_replacement <- ""
  }
  if(resampling_ensemble %in% c(1,2,3)){
    resampling_ensemble <- paste0("--resamplingEnsemble ",as.character(resampling_ensemble))
  }else{
    resampling_ensemble <- ""
  }
  if(save_bootstrap_graphs){
    save_bootstrap_graphs <-"--saveBootstrapGraphs"
  }else{
    save_bootstrap_graphs <- ""
  }

  flags <- c("--numberResampling",
             number_resampling,
             "--percentResampleSize",
             percent_resample_size,
             "--seed",
             seed,
             add_original_dataset,
             resampling_with_replacement,
             resampling_ensemble,
             save_bootstrap_graphs
             )

  return(flags)

}

#' Restarts
#' Should delete this code, it is already part of BOSS and I don't believe it applies to FGES like bootstraping does.
#'
#' Automatically sets --allowInternalRandomness         Allow randomness inside algorithm
#' @param num_restarts --numStarts <integer> The number of restarts, random after the first (default 1)
restarts <- function(num_restarts){

}
