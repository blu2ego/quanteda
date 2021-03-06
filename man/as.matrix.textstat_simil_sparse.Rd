% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textstat_simil.R
\name{as.matrix,textstat_simil_sparse-method}
\alias{as.matrix,textstat_simil_sparse-method}
\alias{as.matrix,textstat_simil_symm_sparse-method}
\title{as.matrix method for textstat_simil_sparse}
\usage{
\S4method{as.matrix}{textstat_simil_sparse}(x, omitted = NA, ...)

\S4method{as.matrix}{textstat_simil_symm_sparse}(x, omitted = NA, ...)
}
\arguments{
\item{x}{an object returned by \link{textstat_simil} when \code{min_simil > 0}}

\item{omitted}{value that will replace the omitted cells}

\item{...}{unused}
}
\value{
a \link{matrix} object
}
\description{
as.matrix method for textstat_simil_sparse
}
\keyword{internal}
\keyword{textstat}
