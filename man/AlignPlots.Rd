% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AlignPlots.R
\name{AlignPlots}
\alias{AlignPlots}
\title{AlignPlots}
\usage{
AlignPlots(
  plots.list = plot.list,
  ref = NULL,
  globalTitle = "",
  keepTitles = FALSE,
  keepLegends = FALSE,
  nb.columns = 1,
  margin.unit = c(0, 0, 0, 0),
  last = TRUE,
  width = TRUE,
  height = TRUE
)
}
\arguments{
\item{plots.list}{The list of ggplot2}

\item{keepLegends}{Whether to keep legends}

\item{margin.unit}{The margins of the plot}

\item{last}{whehter add margin}
}
\value{
a grob list
}
\description{
Creates a plot of the crayon colors in \code{\link{brocolors}}
}
\examples{
groblist <- VAlignPlots(plots.list=list(p.snp,p.highc,p.tx),margin.unit=c(0.1,0,0.1,0),keepLegends = TRUE)


}
\seealso{
AlignPlots
}
\author{
Rui Chen (crotoc@gmail.com)
}
\keyword{plot}
