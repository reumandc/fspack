% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotlcfs.R
\name{plotlcfs}
\alias{plotlcfs}
\title{Plots multiple \code{lcf} objects on the same axes}
\usage{
plotlcfs(lcflist, bds = NULL, xlabel, ylabel, filename = NULL)
}
\arguments{
\item{lcflist}{A list of \code{lcf} objects}

\item{bds}{Plot bounds for the x axis. Default value NULL corresponds to using the minimum range that includes the ranges of all the \code{lcf} objects.}

\item{filename}{File name (without extension) for saving the plot as a pdf. Default NULL uses the default plotting device instead.}
}
\value{
\code{plotlcfs} returns nothing but generates a plot.
}
\description{
Plots multiple \code{lcf} objects on the same axes
}
\examples{
lcf1<-lcf(c(1,2,3,4),c(1,0,1))
lcf2<-lcf(c(1.5,2.5,3.5),c(-1,-2))
h<-list(lcf1,lcf2)
plotlcfs(lcflist=h[1],xlabel="xlab test",ylabel="ylab test")

}
\author{
Daniel Reuman, \email{reuman@ku.edu}
}
