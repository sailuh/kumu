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
#' optimal BIC score will be selected. Random after the first. Defaults to 1.
#' @param time_lag This creates a time-series model automatically with a certain
#' number of lags. Defaults to zero.
#' @param allow_internal_randomness If true, the algorithm allow the algorithm to
#' use certain heuristic random steps. This can improve performance,
#' but may make the algorithm non-deterministic.
#' @param use_bes  True if the final BES (Backward Equivalence Search) step is
#' used from the GES (Greedy Equivalence Search) algorithm.
#' This step is needed for correctness but for large models,
#' since usually nearly all edges are oriented in the CPDAG,
#' it is heurically not needed.
#'
#' @references Dimitris Margaritis and Sebastian Thrun. Bayesian network induction via local neighborhoods. Advances in neural information processing systems, 12, 1999.
#' @references Raskutti, G., & Uhler, C. (2018). Learning directed acyclic graph models based on sparsest permutations. Stat, 7(1), e183.
#' @references Solus, L., Wang, Y., Matejovicova, L., & Uhler, C. (2017). Consistency guarantees for permutation-based causal inference algorithms. arXiv preprint arXiv:1702.03530.
#' @references Lam, W. Y., Andrews, B., & Ramsey, J. (2022, August). Greedy relaxations of the sparsest permutation algorithm. In Uncertainty in Artificial Intelligence (pp. 1052-1062). PMLR.
#'
#'
#' @export
algorithm_boss <- function(num_starts = 1,
                    time_lag = 0,
                    allow_internal_randomness = FALSE,
                    use_bes = FALSE){

  if(allow_internal_randomness){
    allow_internal_randomness <- "--allowInternalRandomness"
  }else{
    allow_internal_randomness <- ""
  }

  if(use_bes){
    use_bes <- "--useBes"
  }else{
    use_bes <- ""
  }

  flags <- c( "--algorithm",
              "boss",
              "--numStarts",
              num_starts,
              "--timeLag",
              time_lag,
              allow_internal_randomness,
              use_bes)

  return(flags)

}
