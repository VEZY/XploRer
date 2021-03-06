% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_MTG.R
\name{parse_MTG_node}
\alias{parse_MTG_node}
\title{Parse MTG node}
\usage{
parse_MTG_node(MTG_node)
}
\arguments{
\item{MTG_node}{An MTG node (e.g. "/Individual0")}
}
\value{
A parsed node in the form of a list of three:
\itemize{
\item the link
\item the symbol
\item and the index
}
}
\description{
Parse MTG nodes (called from \code{\link[=parse_MTG_MTG]{parse_MTG_MTG()}})
}
\keyword{internal}
