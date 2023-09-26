# Kumu - https://github.com/sailuh/kumu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Sem BIC Score
#'
#' Compose the flags of the Sem BIC Score.
#'
#' This is specifically a BIC score for the linear, Gaussian case,
#' where we include an additional penalty term, which is commonly used.
#'
#' We call this the penalty discount. So our formulas has BIC = 2L - ck log N,
#' where L is the likelihood, c the penalty discount (usually greater than or equal to 1),
#' and N the sample size.
#'
#' Since the assumption is that the data are distributed as Gaussian,
#' this reduces to BIC = -n log sigma - ck ln N, where sigma is the standard
#' deviation of the linear residual obtained by regressing a child variable
#' onto all of its parents in the model.
#'
#' @param penalty_discount Penalty discount (min = 0.0)
#' @param sem_bic_rule Lambda: 1 = Chickering, 2 = Nandy
#' @param sem_bic_structure_prior Structure Prior for SEM BIC (default 0)
#' @param precompute_covariances True if covariance matrix should be pre-computed
#' for tubular continuous data
#' @export
score_sem_bic <- function(penalty_discount = 0.0,
                          sem_bic_rule,
                          sem_bic_structure_prior = 0,
                          precompute_covariances){

  if(precompute_covariances){
    precompute_covariances <- "--precomputeCovariances"
  }else{
    precompute_covariances <- ""
  }

  flags <- c("--score",
             "sem-bic-score",
             "--penaltyDiscount",
            penalty_discount,
            "--semBicRule",
            sem_bic_rule,
            "--semBicStructurePrior",
            sem_bic_structure_prior,
            precompute_covariances)

  return(flags)

}
