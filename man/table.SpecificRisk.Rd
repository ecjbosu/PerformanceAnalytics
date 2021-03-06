% Generated by roxygen2 (4.0.1): do not edit by hand
\name{table.SpecificRisk}
\alias{table.SpecificRisk}
\title{Specific risk Summary: Statistics and Stylized Facts}
\usage{
table.SpecificRisk(Ra, Rb, Rf = 0, digits = 4)
}
\arguments{
\item{Ra}{an xts, vector, matrix, data frame, timeSeries or zoo object of
asset returns}

\item{Rb}{return vector of the benchmark asset}

\item{Rf}{risk free rate, in same period as your returns}

\item{digits}{number of digits to round results to}
}
\description{
Table of specific risk, systematic risk and total risk
}
\examples{
data(managers)
table.SpecificRisk(managers[,1:8], managers[,8])

require("Hmisc")
result = t(table.SpecificRisk(managers[,1:8], managers[,8], Rf=.04/12))

textplot(format.df(result, na.blank=TRUE, numeric.dollar=FALSE, cdec=c(3,3,1)),
rmar = 0.8, cmar = 2,  max.cex=.9, halign = "center", valign = "top",
row.valign="center", wrap.rownames=20, wrap.colnames=10,
col.rownames=c("red", rep("darkgray",5), rep("orange",2)), mar = c(0,0,3,0)+0.1)
title(main="Portfolio specific, systematic and total risk")
}
\author{
Matthieu Lestel
}
\references{
Carl Bacon, \emph{Practical portfolio performance measurement
and attribution}, second edition 2008 p.76
}
\seealso{
\code{\link{SystematicRisk}} \cr \code{\link{SpecificRisk}}
\cr \code{\link{TotalRisk}}
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{ts}

