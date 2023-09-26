# Kumu - https://github.com/sailuh/kumu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Knowledge File
#'
#' Compose the flags of the Knowledge File
#'
#' This is a simple function that only compose the flags of the knowledge file.
#' The creation of the knowledge file is not yet implemented and must be done by
#' hand.
#' See https://bd2kccd.github.io/docs/causal-cmd/
#'
#' @param knowledge_file_path The path of the manually created knowledge file
#' @export
knowledge_file_path <- function(knowledge_file_path){

  knowledge_file_path <- path.expand(knowledge_file_path)

  flags <- c("--knowledge",
             knowledge_file_path)
  return(flags)

}
