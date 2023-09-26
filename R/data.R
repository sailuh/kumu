# Kumu - https://github.com/sailuh/kumu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.


#' Data I/O
#'
#' Compose the flags of the input and output data
#'
#' @param dataset_path the dataset path to apply the model
#' @param data_type the type of data
#' @param column_delimiter how columns are defined in the dataset_path file
#' @param output_folder_path Output directory
#' @param is_json_output Write out graph as json.
#' @export
data_io <- function(dataset_path,
                    data_type = "continuous",
                    column_delimiter = "comma",
                    output_folder_path,
                    is_json_output){

  dataset_path <- path.expand(dataset_path)
  output_folder_path <- path.expand(output_folder_path)

  flags <- c("--dataset",
              dataset_path,
              "--data-type",
              data_type,
              "--delimiter",
              column_delimiter,
              "--out",
              output_folder_path)

  if(is_json_output){
    flags <- c(flags,"--json-graph")
  }

  return(flags)

}
