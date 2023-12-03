# Kumu - https://github.com/sailuh/kumu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Algorithm BOSS
#'
#' Compose the flags of the BOSS algorithm
#'
#' BOSS (Best Order Score Search) is an algorithm that, like GRaSP,
#' generalizes and extends the GSP (Greedy Sparsest Permutation) algorithm.
#' It has been tested to 1000 variables with an average degree of 20 and gives
#' near perfect precisions and recalls for N = 10,000
#' (with recall that drop to 0.9 for N = 1000).
#'
#' The algorithms works by building DAGs given permutations in ways similar
#' to those described in Raskutti and Uhler and Solus et al. (see references below)
#'
#' Knowledge of forbidden edges and required edges may be used with this algorithm.
#' Also, knowledge of tiers may be used. If tiered knowledge is supplied,
#' the algorithm will analyze the tiers in order, so that the time required
#' for the algorithm is linear in the number of tiers.
#'
#' For more details, see: https://www.phil.cmu.edu/tetrad-javadocs/7.4.0/edu/cmu/tetrad/search/Boss.html
#' and https://cmu-phil.github.io/tetrad/manual/#boss
#'
#' @param num_starts The number of times the algorithm should be re-run
#' from different random starting permutations. The model with the most
#' optimal BIC score will be selected. Random after the first. Defaults to 1. Integer.
#' @param num_threads The number of threads (>= 1) to use for the search. Integer.
#' @param time_lag This creates a time-series model automatically with a certain
#' number of lags. Defaults to zero.
#' @param use_bes  TRUE if the final BES (Backward Equivalence Search) step is
#' used from the GES (Greedy Equivalence Search) algorithm.
#' This step is needed for correctness but for large models,
#' since usually nearly all edges are oriented in the CPDAG,
#' it is heuristically not needed.
#' @param use_data_order TRUE just in case data variable order should be used for the first initial permutation.
#' @param verbose TRUE if verbose output should be printed or logged
#'
#' @references Dimitris Margaritis and Sebastian Thrun. Bayesian network induction via local neighborhoods. Advances in neural information processing systems, 12, 1999.
#' @references Raskutti, G., & Uhler, C. (2018). Learning directed acyclic graph models based on sparsest permutations. Stat, 7(1), e183.
#' @references Solus, L., Wang, Y., Matejovicova, L., & Uhler, C. (2017). Consistency guarantees for permutation-based causal inference algorithms. arXiv preprint arXiv:1702.03530.
#' @references Lam, W. Y., Andrews, B., & Ramsey, J. (2022, August). Greedy relaxations of the sparsest permutation algorithm. In Uncertainty in Artificial Intelligence (pp. 1052-1062). PMLR.
#'
#'
#' @export
algorithm_boss <- function(num_starts = 1,
                           num_threads = 1,
                           time_lag = 0,
                           use_bes = FALSE,
                           use_data_order = TRUE,
                           verbose = FALSE){

  if(use_bes){
    use_bes <- "--useBes"
  }else{
    use_bes <- ""
  }

  if(use_data_order){
    use_data_order <- "--useDataOrder"
  }else{
    use_data_order <- ""
  }

  if(verbose){
    use_bes <- "--verbose"
  }else{
    verbose <- ""
  }

  flags <- c( "--algorithm",
              "boss",
              "--numStarts",
              as.integer(num_starts),
              "--numThreads",
              as.integer(num_threads),
              "--timeLag",
              as.integer(time_lag),
              use_bes,
              use_data_order,
              verbose)

  return(flags)

}


#' Algorithm FGES
#'
#' Compose the flags of the FGES algorithm
#'
#' Implements the Fast Greedy Equivalence Search (FGES) algorithm.
#' This is an implementation of the Greedy Equivalence Search algorithm,
#' originally due to Chris Meek but developed significantly by Max Chickering.
#' FGES uses with some optimizations that allow it to scale accurately to
#' thousands of variables accurately for the sparse case. The reference for FGES is this:
#' The reference for Chickering's GES is this:
#' Chickering (2002) "Optimal structure identification with greedy search"
#' Journal of Machine Learning Research.
#' FGES works for the continuous case, the discrete case,
#' and the mixed continuous/discrete case, so long as a BIC score is available
#' for the type of data in question.
#' To speed things up, it has been assumed that variables X and Y with
#' zero correlation do not correspond to edges in the graph. This is a
#' restricted form of the heuristic speedup assumption, something GES does not assume.
#' This heuristic speedup assumption needs to be explicitly turned on using setHeuristicSpeedup(true).
#' Also, edges to be added or remove from the graph in the forward or backward phase,
#' respectively are cached, together with the ancillary information needed to do the
#' additions or removals, to reduce rescoring.
#' A number of other optimizations were also. See code for details.
#' This class is configured to respect knowledge of forbidden and required edges,
#' including knowledge of temporal tiers.
#' For more details, see: https://www.phil.cmu.edu/tetrad-javadocs/7.6.0/edu/cmu/tetrad/search/Fges.html
#'
#' @param max_degree Integer. The maximum degree of the graph (min = -1)
#' from different random starting permutations. The model with the most
#' optimal BIC score will be selected. Random after the first. Defaults to 1.
#' @param time_lag This creates a time-series model automatically with a certain
#' number of lags. Defaults to zero.
#' @param faithfulness_assumed TRUE if (one edge) faithfulness should be assumed
#' @param meek_verbose TRUE if verbose output for Meek rule applications should be printed or logged
#' @param parallelized TRUE if the search should be parallelized
#' @param symmetric_first_step TRUE if the first step step for FGES should do scoring for both X->Y and Y->X
#' @param verbose TRUE if verbose output should be printed or logged
#'
#' @references Ramsey, J., Glymour, M., Sanchez-Romero, R., & Glymour, C. (2017). A million variables and more: the fast greedy equivalence search algorithm for learning high-dimensional graphical causal models, with an application to functional magnetic resonance images. International journal of data science and analytics, 3, 121-129.
#'
#'
#' @export
algorithm_fges <- function(max_degree = 1000,
                           time_lag = 0,
                           faithfulness_assumed = FALSE,
                           meek_verbose = FALSE,
                           parallelized = FALSE,
                           symmetric_first_step = FALSE,
                           verbose = FALSE){

  if(faithfulness_assumed){
    faithfulness_assumed <- "--faithfulnessAssumed"
  }else{
    faithfulness_assumed <- ""
  }

  if(meek_verbose){
    meek_verbose <- "--meekVerbose"
  }else{
    meek_verbose <- ""
  }

  if(parallelized){
    parallelized <- "--parallelized"
  }else{
    parallelized <- ""
  }

  if(symmetric_first_step){
    symmetric_first_step <- "--symmetricFirstStep"
  }else{
    symmetric_first_step <- ""
  }

  if(verbose){
    verbose <- "--verbose"
  }else{
    verbose <- ""
  }

  flags <- c( "--algorithm",
              "fges",
              "--maxDegree",
              as.integer(max_degree),
              "--timeLag",
              as.integer(time_lag),
              faithfulness_assumed,
              meek_verbose,
              parallelized,
              symmetric_first_step,
              verbose)

  return(flags)

}
