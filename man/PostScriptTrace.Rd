\name{PostScriptTrace}
\alias{PostScriptTrace}
\title{ Convert PostScript file to RGML file }
\description{
  Converts a PostScript file into an RGML file, which is
  an XML document describing an image that can be read into R.
}
\usage{PostScriptTrace(file, outfilename, charpath=TRUE)}
\arguments{
  \item{file}{The name of the PostScript file.}
  \item{outfilename}{The name of the XML document.}
  \item{charpath}{A boolean indicating whether text in the PostScript
    file should be converted to vector outlines, or left as just text.}
}
\details{
  This function calls ghostscript to do the conversion,
  so will only work if ghostscript is installed on your system.

  If text is converted to outlines, it can be drawn as outlines,
  or filled using a crude algorithm which may or may not work
  (see \code{grid.picture}).  On the other hand, if text is left
  as just text, font information is not stored so the text may
  not end up looking much like the original.
}
\references{
  \url{http://www.cs.wisc.edu/~ghost/}
}
\keyword{dplot}
