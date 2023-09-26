# Kumu - https://github.com/sailuh/kumu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Tetrad
#'
#' Base interface for Tetrad Cmd
#'
#' @param tetrad_cmd_path path to tetrad-cmd
#' @param data_flags use a function in R/score.R
#' @param knowledge_flags use a function in R/knowledge.R
#' @param algorithm_flags use a function in R/algorithm.R
#' @param score_flags use a function in R/score.R
#' @param bootstrapping_flags use a function in R/bootstrapping.R
#'
#'
#
#' @export
#'
tetrad <- function(tetrad_cmd_path,
                   data_flags,
                   knowledge_flags = list(),
                   algorithm_flags,
                   score_flags,
                   bootstrapping_flags){

  tetrad_cmd_path <- path.expand(tetrad_cmd_path)

  if(length(knowledge_flags) == 0){
    knowledge_flags <- ""
  }

  out <- system2(command = "java",
          args = c("-jar",
                   tetrad_cmd_path,
                   data_flags,
                   knowledge_flags,
                   algorithm_flags,
                   score_flags,
                   bootstrapping_flags),
          stdout = TRUE
  )
  return(out)
}



