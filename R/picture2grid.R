
# Convert graphics-system-neutral "picture" into grid gList

# Viewport from picture
pictureVP <- function(picture, exp=0.05, xscale=NULL, yscale=NULL, ...) {
    if (is.null(xscale) || is.null(yscale)) {
        xscale <- picture@summary@xscale
	yscale <- picture@summary@yscale
    }
    xscale <- xscale + exp*c(-1, 1)*diff(range(xscale))
    yscale <- yscale + exp*c(-1, 1)*diff(range(yscale))
    vpStack(viewport(name="picture.shape", ...,
                     layout=grid.layout(1, 1,
                       widths=abs(diff(xscale)),
                       heights=abs(diff(yscale)),
                       respect=TRUE)),
            viewport(name="picture.scale",
                     layout.pos.col=1,
                     xscale=xscale,
                     yscale=yscale))
}

##################
# Convert picture or path into single grob
# For using picture as a one-off (e.g., plot background)
##################
# Generic grobify() function
setGeneric("grobify",
           function(object, ...) {
               standardGeneric("grobify")
           })

# Individual path converted into grob
setMethod("grobify", signature(object="PictureStroke"),
          function(object, ..., use.gc=TRUE) {
              if (use.gc) {
                  linesGrob(object@x, object@y, default.units="native",
                            gp=gpar(lwd=object@lwd, col=object@rgb), ...)
              } else {
                  linesGrob(object@x, object@y, default.units="native", ...)
              }
          })

setMethod("grobify", signature(object="PictureFill"),
          function(object, ..., use.gc=TRUE) {
              if (use.gc) {
                  polygonGrob(object@x, object@y, default.units="native",
                              gp=gpar(col=NA, fill=object@rgb), ...)
              } else {
                  polygonGrob(object@x, object@y, default.units="native", ...)
              }
          })

pictureHull <- function(object) {
    allx <- unlist(lapply(object@paths, function(x) { x@x }))
    ally <- unlist(lapply(object@paths, function(x) { x@y }))
    hull <- chull(allx, ally)
    list(x=allx[hull], y=ally[hull])
}

setMethod("grobify", signature(object="Picture"),
          function(object,
                   x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                   width=unit(1, "npc"), height=unit(1, "npc"),
                   just="centre", xscale=NULL, yscale=NULL, exp=0.05,
                   FUN=grobify, ..., name=name, gp=gpar()) {
              gTree(childrenvp=pictureVP(object,
                      exp=0.05, xscale=xscale, yscale=yscale,
                      x=x, y=y, width=width, height=height,
                      just=just, gp=gp),
                    children=do.call("gList",
                      lapply(object@paths, FUN,
                             vp=vpPath("picture.shape", "picture.scale"),
                             ...)),
                    # FIXME: editDetails method for "picture" class
                    # to update these bounds if picture grob edited?
                    # (or at least warn that bounds are not updated)
                    hull=pictureHull(object),
                    name=name, cl="picture")
          })

# grobX, grobY methods for gTree of class "picture"
xDetails.picture <- function(x, theta) {
    # Generate a polygon based on picture convex hull
    # NOTE: make sure it has same vpPath as actual paths
    grobX(polygonGrob(x$hull$x, x$hull$y,
                      default.units="native",
                      vp=vpPath("picture.shape", "picture.scale")),
          theta)
}

yDetails.picture <- function(x, theta) {
    # Generate a polygon based on picture convex hull
    # NOTE: make sure it has same vpPath as actual paths
    grobY(polygonGrob(x$hull$x, x$hull$y,
                      default.units="native",
                      vp=vpPath("picture.shape", "picture.scale")),
          theta)
}

##################
# Convert picture or path into "multiple" grob with no viewports
# For using picture as data symbol
##################
setGeneric("symbolize",
           function(object,
                    x=unit(0.5, "npc"),
                    y=unit(0.5, "npc"),
                    size=unit(1, "npc"),
                    units="npc", ...) {
               standardGeneric("symbolize")
           })

symbolLocn <- function(object, x, y, size, units,
                       xscale, yscale) {
    n <- max(length(x), length(y))
    x <- rep(x, length.out=n)
    y <- rep(y, length.out=n)
    if (!is.unit(x))
        x <- unit(x, units)
    if (!is.unit(y))
        y <- unit(y, units)
    if (!is.unit(size))
        size <- unit(size, units)
    # Do everything in INCHES to avoid too much calculation on units
    x <- convertX(x, "inches", valueOnly=TRUE)
    y <- convertY(y, "inches", valueOnly=TRUE)
    sizew <- convertWidth(size, "inches", valueOnly=TRUE)
    sizeh <- convertHeight(size, "inches", valueOnly=TRUE)
    if (is.null(xscale))
        rx <- range(object@x)
    else
        rx <- xscale
    if (is.null(yscale))
        ry <- range(object@y)
    else
        ry <- yscale
    # Determine width and height from size and scale
    # so that the largest scale dimension
    # is given the smallest physical dimension
    sizeAspect <- sizeh / sizew
    scaleAspect <- diff(ry) / diff(rx)
    if (scaleAspect < sizeAspect) {
        width <- sizew
        height <- sizew*scaleAspect
    } else {
        height <- sizeh
        width <- sizeh/scaleAspect
    }
    # Scale object@x/y to [-0.5, 0.5]
    # and then multiply by width/height
    wx <- rep((object@x - mean(rx))/abs(diff(rx)), n)*width
    hy <- rep((object@y - mean(ry))/abs(diff(ry)), n)*height
    # Replicate x/y by length object@x/y
    # NOTE object@x and object@y have same length
    xx <- rep(x, rep(length(object@x), n))
    yy <- rep(y, rep(length(object@x), n))
    list(x=xx + wx, y=yy + hy, n=n)
}

drawDetails.symbolStroke <- function(x, recording) {
    locn <- symbolLocn(x$object, x$x, x$y, x$size,
                       x$units, x$xscale, x$yscale)
    # Create id to distinguish separate symbols
    id <- rep(1:locn$n, each=length(x$object@x))
    # Generate grob representing symbols
    if (x$use.gc) {
        do.call("grid.polyline",
                c(list(x=locn$x, y=locn$y, id=id,
                       default.units="inches",
                       gp=gpar(lwd=x$object@lwd, col=x$object@rgb)),
                  x$poly.args))
    } else {
        do.call("grid.polyline",
                c(list(x=locn$x, y=locn$y, id=id,
                       default.units="inches"),
                  x$poly.args))
    }
}
    
drawDetails.symbolFill <- function(x, recording) {
    locn <- symbolLocn(x$object, x$x, x$y, x$size,
                       x$units, x$xscale, x$yscale)
    # Create id to distinguish separate symbols
    id <- rep(1:locn$n, each=length(x$object@x))
    # Generate grob representing symbols
    if (x$use.gc) {
        do.call("grid.polygon",
                c(list(x=locn$x, y=locn$y, id=id, 
                       default.units="inches",
                       gp=gpar(col=NA, fill=x$object@rgb)),
                  x$poly.args))
    } else {
        do.call("grid.polygon",
                c(list(x=locn$x, y=locn$y, id=id,
                       default.units="inches"),
                  x$poly.args))
    }
}
    
setMethod("symbolize", signature(object="PictureStroke"),
          function(object,
                   x=unit(0.5, "npc"),
                   y=unit(0.5, "npc"),
                   size=unit(1, "npc"),
                   units="npc",
                   xscale=NULL, yscale=NULL, ..., use.gc=TRUE) {
              grob(object=object, x=x, y=y, size=size,
                   units=units, xscale=xscale, yscale=yscale,
                   use.gc=use.gc, poly.args=list(...),
                   cl="symbolStroke")
          })

setMethod("symbolize", signature(object="PictureFill"),
          function(object,
                   x=unit(0.5, "npc"),
                   y=unit(0.5, "npc"),
                   size=unit(1, "npc"),
                   units="npc",
                   xscale=NULL, yscale=NULL, ..., use.gc=TRUE) {
              grob(object=object, x=x, y=y, size=size,
                   units=units, xscale=xscale, yscale=yscale,
                   use.gc=use.gc, poly.args=list(...),
                   cl="symbolFill")
          })

setMethod("symbolize", signature(object="Picture"),
          function(object, 
                   x=unit(0.5, "npc"),
                   y=unit(0.5, "npc"),
                   size=unit(1, "npc"),
                   units="npc",
                   ..., name=name, gp=gpar()) {
              gTree(children=do.call("gList",
                      lapply(object@paths, symbolize,
                             x, y, size, units,
                             xscale=object@summary@xscale,
                             yscale=object@summary@yscale,
                             ...)),
                    name=name, gp=gp)
          })

##################
# Draw entire picture
# or individual paths
##################
pictureGrob <- function(picture, 
                        x=0.5, y=0.5, width=1, height=1,
			just="centre", 
			exp=0.05, xscale=NULL, yscale=NULL,
                        FUN=grobify, ...,
                        name=NULL, gp=gpar()) {
    grobify(picture, 
            x=x, y=y, width=width, height=height, just=just,
            exp=exp, xscale=xscale, yscale=yscale, FUN=FUN,
            ..., name=name, gp=gp)
}

grid.picture <- function(...) {
    grid.draw(pictureGrob(...))
}

symbolsGrob <- function(picture,
                        x=unit(0.5, "npc"),
                        y=unit(0.5, "npc"),
                        size=unit(1, "npc"),
                        units="npc",
                        ...,
                        name=NULL, gp=gpar()) {
    symbolize(picture,
              x=x, y=y, size=size, units=units,
              ..., name=name, gp=gp)
}

grid.symbols <- function(...) {
    grid.draw(symbolsGrob(...))
}
                       
picturePaths <- function(picture,
                         nr, nc,
                         bg="light grey", 
                         xscale=NULL, yscale=NULL,	      
                         label=function(n) { 
                             tg <- textGrob(n, x=0, y=0, 
                                            just=c("left", "bottom"),
                                            gp=gpar(fontsize=6))
                             grid.rect(x=0, y=0, height=unit(6, "points"),
                                       width=grobWidth(tg),
                                       just=c("left", "bottom"),
                                       gp=gpar(fill="white"))
                             grid.draw(tg) },
                         use.gc=TRUE) {
    if (missing(nr) || missing(nc)) {
      nrnc <- n2mfrow(picture@summary@numPaths)
      nr <- nrnc[1]
      nc <- nrnc[2]
    }
    if (is.null(xscale) || is.null(yscale)) {
        xscale <- picture@summary@xscale
	yscale <- picture@summary@yscale
    }
    pushViewport(viewport(layout=grid.layout(nr, nc,
                            widths=rep(diff(range(xscale)),
                              nc),
                            heights=rep(diff(range(yscale)),
                              nr),
                            respect=TRUE)))
    for (i in 1:nr) {
        for (j in 1:nc) {
            pnum <- (i - 1)*nc + j
            if (pnum <= picture@summary@numPaths) {
                pushViewport(viewport(layout.pos.col=j,
                                      layout.pos.row=i),
                             viewport(width=0.95, height=0.95,
                                      xscale=xscale,
                                      yscale=yscale))
                grid.rect(gp=gpar(fill=bg))
                label(pnum)
                grid.draw(grobify(picture@paths[[pnum]], use.gc=use.gc))
                popViewport(2)
            }
        }
    }
    popViewport()
}

