\name{picturePaths}
\alias{picturePaths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw Individual Picture Paths }
\description{
  This function individually draws each path from a \code{"Picture"} object.
}
\usage{
picturePaths(picture, nr, nc,
             bg = "light grey",
             xscale = NULL, yscale = NULL,
             label = function(n) {
               tg <- textGrob(n, x = 0, y = 0,
                              just = c("left", "bottom"),
                              gp = gpar(fontsize = 6))
               grid.rect(x = 0, y = 0,
                         height = unit(6, "points"),
                         width = grobWidth(tg),
                         just = c("left", "bottom"),
                         gp = gpar(fill = "white"))
               grid.draw(tg)
             }, use.gc = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{picture}{ A \code{"Picture"} object. }
  \item{nr}{ Number of rows of paths to draw. }
  \item{nc}{ Number of columns of paths to draw. }
  \item{bg}{ Background colour drawn behind each path. }
  \item{xscale}{A numeric vector of length two indicating the minimum and
    maximum on the x-scale.}
  \item{yscale}{A numeric vector of length two indicating the minimum
    and maximum on the y-scale.}
  \item{label}{ Function to draw a label on each path. }
  \item{use.gc}{ A boolean indicating whether to use the graphical
    parameter information in the path. }
}
\author{ Paul Murrell }
\seealso{
  \code{\link{grid.picture}}
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
