\name{PostScriptTrace}
\alias{PostScriptTrace}
\title{ Convert PostScript file to RGML file }
\description{
  Converts a PostScript file into an RGML file, which is
  an XML document describing an image that can be read into R.
}
\usage{PostScriptTrace(file)}
\arguments{
  \item{file}{The name of the PostScript file.}
}
\details{
  This function calls ghostscript to do the conversion,
  so will only work if ghostscript is installed on your system.
}
\references{
  \url{http://www.cs.wisc.edu/~ghost/}
}
\keyword{dplot}
