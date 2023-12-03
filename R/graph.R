# Kumu - https://github.com/sailuh/kumu
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Parse JSON Graph
#'
#' Parses a graph output by Tetrad CMD or exported from Tetrad GUI
#' as a named list of 3 DTs: "nodes", "ensemble", and "edgeset". If
#' the generated graph does not result from bootstrapping or or
#' multiple runs, then "edgeset" will be an empty list.
#'
#' In Tetrad, Causal algorithms can output a graph as a result of
#' a single run, or multiple runs. When utilizing a single run,
#' the "nodes" and "edgeset" will contain the expected graph
#' as an edgelist table and its nodes.
#'
#' A graph output resulting from multiple runs can be obtained
#' by using either bootstrap or the restart flags. The bootstrap
#' command, as the name implies, performs multiple causal searches
#' over samples of the full dataset. The restart command, however,
#' performs causal searches on the full dataset every time with
#' random initialization (requires random initialization flag).
#'
#' If multiple runs are used, then the graph data generated in the
#' JSON will contain an additional field, "edgeTypeProbabilities",
#' for every edge in "edgeset". The "edgeTypeProbabilities" counts
#' for a given edge in "edgeset", the number of edges
#' in a given direction and their properties, and/or the absence of
#' them of every run. The resulting "edgeset" is thus the ensemble
#' edge derived from the edgeset. For example, suppose across 1000 runs
#' for node1 and node2 we obtain 4 types of edges:
#' * ta: properties = (pd, pl), probability: ~0.48
#' * at: properties = (dd,nl), probability: ~ 0.37
#' * tt: properties = (), probability: ~ 0.13
#' * nil: properties = (), probability: ~ 0.01
#'
#' In such edgeTypeProbabilities for node1 and node2, the reported
#' edgeset for node1 and node 2 is ta, the properties are (pd, pl)
#' and the probability 0.48 + 0.37 + 0.13 ~= 0.98. Observe the final
#' reported property for the node1,node2 pair is thus the highest
#' probability of the edgeset (assuming ensemble preserved), however
#' the probability is the sum of the indvidual probabilities (except for
#' the nil case, which were causal search runs which resulted in no edges
#' being formed).
#'
#' The label "ta" stands for (t)ail and (a)rrow. For a list of other
#' acronyms, (pd, nl, etc.) refer to Kumu GitHub Wiki or Tetrad Docs.
#'
#'
#' @param graph_filepath The path to the JSON graph file
#' @return A named list of 3 DTs: "nodes", "edgeset", and
#' "edge_type_probabilities".
#' @export
parse_graph <- function(graph_filepath){

  parse_edge_type_probabilities <- function(edge_type_probabilities){

    parse_edge_type_probability <- function(edge_type_probability){
      edge_type <- edge_type_probability$edgeType
      properties <- stringi::stri_c(edge_type_probability$properties,collapse = ";")
      probability <- edge_type_probability$probability
      dt <- data.table(edge_type,properties,probability)
      return(dt)
    }

    edge_type_probabilities <- rbindlist(lapply(edge_type_probabilities,parse_edge_type_probability))
    edge_type_probabilities[properties == ""]$properties <- NA
    return(edge_type_probabilities)
  }

  graph <- jsonlite::read_json(graph_filepath)

  # Parse Node DT
  parsed_graph <- list()
  parsed_graph[["nodes"]] <- sapply(graph[["nodes"]],function(x) x[["name"]])
  parsed_graph[["nodes"]] <- data.table(node_name = parsed_graph[["nodes"]])

  # The EdgeSet field can contain the edgeTypeProbabilities field if multiple runs
  # were performed on the causal search. # The EdgeSet field is simply the usual
  # edgelist in a graph. The edgeTypeProbabilities stores the counts of every type
  # of edge formed across all the runs as probabilities.

  # To avoid duplicate rows on the EdgeSet table, the EdgeSet and
  # edgeTypeProbabilities are stored as separate tables which can be
  # inner joined if necessary.
  parse_graph_edgeset <- function(graph_edgeset){

    # Parse the EdgeSet information (i.e. the edgelist that exists whether single or multiple run)
    node1_name <- graph_edgeset$node1$name
    node2_name <- graph_edgeset$node2$name
    endpoint1 <-  graph_edgeset$endpoint1
    endpoint2 <-  graph_edgeset$endpoint2
    bold <- graph_edgeset$bold
    highlighted <- graph_edgeset$highlighted
    properties <- stringi::stri_c(graph_edgeset$properties,collapse = ";")
    probability <- graph_edgeset$probability

    edgeset_dt <- data.table(node1_name,
                             node2_name,
                             endpoint1,
                             endpoint2,
                             bold,
                             highlighted,
                             properties,
                             probability)


    # Parse the edgeTypeProbabilities field which contains various types of edges
    graph_edgeset_list <- list()
    # Only populated if the graph resulted from multiple runs
    if(length(graph_edgeset[["edgeTypeProbabilities"]]) > 0){
      edge_type_probabilities_dt <- parse_edge_type_probabilities(graph_edgeset[["edgeTypeProbabilities"]])
      edge_type_probabilities_dt$node1_name <- node1_name
      edge_type_probabilities_dt$node2_name <- node2_name
      edge_type_probabilities_dt <- edge_type_probabilities_dt[,.(node1_name,
                                                                  node2_name,
                                                                  edge_type,
                                                                  properties,
                                                                  probability)]
      graph_edgeset_list[["edge_type_probabilities"]] <- edge_type_probabilities_dt
    }else{
      graph_edgeset_list[["edge_type_probabilities"]] <- data.table()
    }

    graph_edgeset_list[["edgeset"]] <- edgeset_dt


    # Return one edge ensemble and the set of edges as separate tables
    return(graph_edgeset_list)
  }

  # Extract as a list of two data tables all edge ensemble and sets
  # Note this list has per element only one edge ensemble and one edge set per list element
  # Therefore there are as many list elements are there are edge sets at this point
  edgesets_list <- lapply(graph[["edgesSet"]],parse_graph_edgeset)

  # Rbind the entire list for the edge ensemble DT
  edgeset_dt <- rbindlist(lapply(edgesets_list,"[[","edgeset"))
  # Likewise, Rbind the entire list for the edge sets DT
  edge_type_probabilities_dt <- rbindlist(lapply(edgesets_list,"[[","edge_type_probabilities"))
  graph_edgesets <- list()

  parsed_graph[["edgeset"]] <- edgeset_dt
  parsed_graph[["edge_type_probabilities"]] <- edge_type_probabilities_dt

  # The graph is a list of only 3 elements, carrying the 3 DTs.
  return(parsed_graph)
}

# Various imports
utils::globalVariables(c("."))
#' @importFrom magrittr %>%
#' @importFrom stringi stri_replace_last
#' @importFrom stringi stri_replace_first
#' @importFrom stringi stri_match_all
#' @importFrom stringi stri_match_first_regex
#' @importFrom stringi stri_detect_regex
#' @importFrom stringi stri_c
#' @importFrom stringi stri_split_regex
#' @importFrom data.table data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table .N
#' @importFrom data.table :=
#' @importFrom data.table rbindlist
#' @importFrom data.table setkey
#' @importFrom data.table setkeyv
#' @importFrom data.table setnames
NULL
