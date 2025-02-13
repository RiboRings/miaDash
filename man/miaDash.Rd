% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miaDash.R
\name{miaDash}
\alias{miaDash}
\title{miaDash}
\usage{
miaDash()
}
\value{
An \code{\link[iSEE:iSEE]{iSEE}} app with a custom landing page to
build TreeSE objects and explore \link[mia:mia-datasets]{mia datasets}.
}
\description{
miaDash is a web app that provides an interface to build and explore
\code{\link[TreeSummarizedExperiment:TreeSummarizedExperiment-constructor]{TreeSummarizedExperiment}}
(TreeSE) objects by means of \link[iSEE:iSEE]{iSEE}.
}
\examples{
app <- miaDash()

if (interactive()) {
  shiny::runApp(app)
}

}
\seealso{
\link[iSEE:iSEE]{iSEE} \link[mia:mia]{mia}
\link[miaViz:miaViz]{miaViz}
}
