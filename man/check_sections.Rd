% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_MTG.R
\name{check_sections}
\alias{check_sections}
\title{Check sections from MTG}
\usage{
check_sections(MTG_file)
}
\value{
An error if a section is missing or not properly ordered, nothing otherwise
}
\description{
Test if the sections are all present in the MTG file, and written in
the right order.
}
\examples{
\dontrun{
check_sections(mtg)
}
}
\keyword{internal}
