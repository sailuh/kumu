% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.R
\name{parse_graph}
\alias{parse_graph}
\title{Parse JSON Graph}
\usage{
parse_graph(graph_filepath)
}
\arguments{
\item{graph_filepath}{The path to the JSON graph file}
}
\value{
A named list of 3 DTs: "nodes", "edgeset", and
"edge_type_probabilities".
}
\description{
Parses a graph output by Tetrad CMD or exported from Tetrad GUI
as a named list of 3 DTs: "nodes", "ensemble", and "edgeset". If
the generated graph does not result from bootstrapping or or
multiple runs, then "edgeset" will be an empty list.
}
\details{
In Tetrad, Causal algorithms can output a graph as a result of
a single run, or multiple runs. When utilizing a single run,
the "nodes" and "edgeset" will contain the expected graph
as an edgelist table and its nodes.

A graph output resulting from multiple runs can be obtained
by using either bootstrap or the restart flags. The bootstrap
command, as the name implies, performs multiple causal searches
over samples of the full dataset. The restart command, however,
performs causal searches on the full dataset every time with
random initialization (requires random initialization flag).

If multiple runs are used, then the graph data generated in the
JSON will contain an additional field, "edgeTypeProbabilities",
for every edge in "edgeset". The "edgeTypeProbabilities" counts
for a given edge in "edgeset", the number of edges
in a given direction and their properties, and/or the absence of
them of every run. The resulting "edgeset" is thus the ensemble
edge derived from the edgeset. For example, suppose across 1000 runs
for node1 and node2 we obtain 4 types of edges:
* ta: properties = (pd, pl), probability: ~0.48
* at: properties = (dd,nl), probability: ~ 0.37
* tt: properties = (), probability: ~ 0.13
* nil: properties = (), probability: ~ 0.01

In such edgeTypeProbabilities for node1 and node2, the reported
edgeset for node1 and node 2 is ta, the properties are (pd, pl)
and the probability 0.48 + 0.37 + 0.13 ~= 0.98. Observe the final
reported property for the node1,node2 pair is thus the highest
probability of the edgeset (assuming ensemble preserved), however
the probability is the sum of the indvidual probabilities (except for
the nil case, which were causal search runs which resulted in no edges
being formed).

The label "ta" stands for (t)ail and (a)rrow. For a list of other
acronyms, (pd, nl, etc.) refer to Kumu GitHub Wiki or Tetrad Docs.
}
