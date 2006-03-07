\name{readPicture}
\alias{readPicture}
\title{ Import an RGML file }
\description{
  This function reads in an RGML file (produced by
  \code{\link{PostScriptTrace}}) and creates a
  \code{"Picture"} object.
}
\usage{
readPicture(rgmlFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rgmlFile}{ The name of the RGML file. }
}
\value{
  An object of class \code{"Picture"}.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{PostScriptTrace}}
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
