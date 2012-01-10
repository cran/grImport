
library(Sxslt)

roughRGML <- xsltApplyStyleSheet("setosa.svg",
                                 "/home/staff/paul/Research/Rstuff/Import/grimport/inst/xsl/svg2rgml.xsl")

# Calculate summary from paths
calcSummary <- function(doc) {
    paths <- getNodeSet(doc, "//rgml:path")
    points <- getNodeSet(doc, "//rgml:move | //rgml:line")
    x <- as.numeric(sapply(points, function(p) xmlAttrs(p)["x"]))
    y <- as.numeric(sapply(points, function(p) xmlAttrs(p)["y"]))
    list(newXMLNode("summary",
                    attrs=c(count=length(paths),
                            xmin=min(x), xmax=max(x),
                            ymin=min(y), ymax=max(y))))
}

rgml <- addChildren(getNodeSet(roughRGML$doc, "//rgml:picture")[[1]],
                    kids=calcSummary(roughRGML$doc))

saveXML(rgml, "temp.xml")

library(grImport)
temp <- readPicture("temp.xml")
grid.picture(temp)
