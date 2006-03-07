# Function to extract outlines from PostScript paths

PScaptureText <- c()

PScaptureChars <- c()
    
PScaptureHead <- function(charpath) {
    c("%!PS-Adobe-2.0 EPSF-1.2",
      "%%BeginProcSet:convertToR 0 0",
      
      # XML file header info
      "(<?xml version='1.0'?>\n\n) print",
      "(<picture xmlns:rgml = 'http://r-project.org/RGML'>\n\n) print",

      # useful definitions
      # Define my counters GLOBALLY so the postscript
      # file I run cannot reset my counters with a restore!
      "true setglobal",
      "/convertToR dup 100 dict def load begin",
      "/str 20 string def",
      "/id 1 def",
      # Do these need to be even "larger"?
      "/xmax -99999 def",
      "/xmin  99999 def",
      "/ymax -99999 def",
      "/ymin  99999 def",
                   
      # path processing
      "/mymove {",
      "  (\t<move) print",
      "  matrix currentmatrix",
      "  transform",
      "  dup",
      "  convertToR exch /ystart exch put",
      "  dup",
      "  convertToR exch /cury exch put",
      "  dup",
      "  convertToR /ymin get lt {convertToR /ymin cury put} if",
      "  dup",
      "  convertToR /ymax get gt {convertToR /ymax cury put} if",
      "  ( y=') print str cvs print (') print",
      "  dup",
      "  convertToR exch /xstart exch put",
      "  dup",
      "  convertToR exch /curx exch put",
      "  dup",
      "  convertToR /xmin get lt {convertToR /xmin curx put} if",
      "  dup",
      "  convertToR /xmax get gt {convertToR /xmax curx put} if",
      "  ( x=') print str cvs print (') print",
      "  (/>\n) print",
      "} def",
      "/myline {",
      "  (\t<line) print",
      "  matrix currentmatrix",
      "  transform",
      "  dup",
      "  convertToR exch /cury exch put",
      "  dup",
      "  convertToR /ymin get lt {convertToR /ymin cury put} if",
      "  dup",
      "  convertToR /ymax get gt {convertToR /ymax cury put} if",
      "  ( y=') print str cvs print (') print",
      "  dup",
      "  convertToR exch /curx exch put",
      "  dup",
      "  convertToR /xmin get lt {convertToR /xmin curx put} if",
      "  dup",
      "  convertToR /xmax get gt {convertToR /xmax curx put} if",
      "  ( x=') print str cvs print (') print",
      "  (/>\n) print",
      "  } def",
      "/mycurve {",
      "  (curve ) print",
      "  str cvs print ( ) print",
      "  str cvs print (\n) print",
      "  } def",
      "/myclose {",
      # Convert 'closepath' to 'lineto'
      # "  (\t<close/>\n) print",
      "  (\t<line) print",
      "  ( y=') print convertToR /ystart get str cvs print (') print",
      "  ( x=') print convertToR /xstart get str cvs print (') print",
      "  (/>\n) print",
      "  } def",
      
      # echoing graphics state
      "/printcol {",
      "  currentrgbcolor",
      "  (\t\t<rgb) print",
      "  ( r=') print 2 index str cvs print (') print",
      "  ( g=') print 1 index str cvs print (') print",
      "  ( b=') print str cvs print (') print",
      "  (/>\n) print",
      "  pop pop",
      "  } def",
      "/printlwd {",
      "  currentlinewidth",
      "  ( lwd=') print 1.33 mul str cvs print (') print",
      "} def",
      "/printstyle {",
      "  (\t\t<style) print",
      "  printlwd",
      "  (/>\n) print",
      "} def",

      # print out "closestroke" marker plus graphics state info
      # make sure the colour is RGB not BGR
      # multiply line width by 1.33
      # (R converts 1 to 0.75 when writing PostScript)
      "/mystroke {",
      "  (<path type='stroke') print",
      "  ( id=') print convertToR /id get str cvs print ('>\n) print",
      "  (\t<context>\n) print",
      "  printcol  ",
      "  printstyle",
      "  (\t</context>\n\n) print",
      "  pathforall",
      "  convertToR /id get 1 add convertToR exch /id exch put",
      "  (</path>\n\n) print",
      "} def",
      "/myfill {",
      "  (<path type='fill') print",
      "  ( id=') print convertToR /id get str cvs print ('>\n) print",
      "  (\t<context>\n) print",
      "  printcol",
      "  printstyle",
      "  (\t</context>\n\n) print",
      "  pathforall",
      "  convertToR /id get 1 add convertToR exch /id exch put",
      "  (</path>\n\n) print",
      "} def",
      "/mytext {",
      "  (<path type='text') print",
      "  ( id=') print convertToR /id get str cvs print (') print",
      "  ( string=') print dup print (') print",
      # (x, y) location of text
      "  currentpoint",
      "  matrix currentmatrix",
      "  transform",
      "    dup",
      "    convertToR exch /cury exch put",
      "    dup",
      "    convertToR /ymin get lt {convertToR /ymin cury put} if",
      "    dup",
      "    convertToR /ymax get gt {convertToR /ymax cury put} if",
      "    ( y=') print str cvs print (') print",
      "    dup",
      "    convertToR exch /curx exch put",
      "    dup",
      "    convertToR /xmin get lt {convertToR /xmin curx put} if",
      "    dup",
      "    convertToR /xmax get gt {convertToR /xmax curx put} if",
      "    ( x=') print str cvs print (') print",
      # (width, height) of text
      "  dup true charpath flattenpath mark pathbbox",
      "  ( width=') print",
      # Calculate width
      "    1 index 4 index sub",
      # Add to current x for checking against xmin/xmax
      "    dup currentpoint pop add",
      # Get currentpoint[y] so can transform 
      "    currentpoint exch pop", # Drop currentpoint[x]
      "    matrix currentmatrix",
      "    transform pop", # Drop (transformed) currentpoint[y]
      "    dup",
      "    convertToR exch /curx exch put",
      "    dup",
      "    convertToR /xmin get lt {convertToR /xmin curx put} if",
      "    convertToR /xmax get gt {convertToR /xmax curx put} if",      
      # Transform original width (use dummy 0 for y)
      "    0 matrix currentmatrix",
      "    transform pop", # Drop (transformed) dummy y 
      "    str cvs print (') print",
      "  ( height=') print",
      # Calculate height
      "    2 index sub",
      # Add to current y for checking against ymin/ymax
      "    dup currentpoint exch pop add",
      # Get currentpoint[x] so can transform 
      "    currentpoint pop exch", # Drop currentpoint[y]
      "    matrix currentmatrix",
      "    transform exch pop", # Drop (transformed) currentpoint[x]
      "    dup",
      "    convertToR exch /cury exch put",
      "    dup",
      "    convertToR /ymin get lt {convertToR /ymin cury put} if",
      "    convertToR /ymax get gt {convertToR /ymax cury put} if",      
      # Transform original height (use dummy 0 for x)
      "    0 exch matrix currentmatrix",
      "    transform exch pop", # Drop (transformed) dummy x 
      "    str cvs print (') print",
      "  cleartomark",
      "  (>\n) print",
      "  (\t<context>\n) print",
      "  printcol",
      "  printstyle",
      "  (\t</context>\n\n) print",
      "  convertToR /id get 1 add convertToR exch /id exch put",
      "  (</path>\n\n) print",
      "} def",
      "/mychar {",
      "  (<path type='char') print",
      "  ( id=') print convertToR /id get str cvs print ('>\n) print",
      "  pathforall",
      "  (\t<context>\n) print",
      "  printcol",
      "  printstyle",
      "  (\t</context>\n\n) print",
      "  convertToR /id get 1 add convertToR exch /id exch put",
      "  (</path>\n\n) print",
      "} def",

      # override paint operators
      "/stroke {",
      "  flattenpath {mymove} {myline} {mycurve} {myclose}",
      "  mystroke",
      "  newpath",
      "} def",
      "/fill {",
      "  flattenpath {mymove} {myline} {mycurve} {myclose}",
      "  myfill",
      "  newpath",
      "} def",
      "/eofill {",
      "  flattenpath {mymove} {myline} {mycurve} {myclose}",
      "  myfill",
      "  newpath",
      "} def",
      # text is split into individual characters,
      # each character is converted to a path, flattened
      # and then stroked
      "/strokechar {",
      "  exch dup 3 -1 roll",
      "  1 getinterval", 
      "  true charpath flattenpath",
      "  {mymove} {myline} {mycurve} {myclose}",
      "  mychar",
      # Save current location (starting position for next char),
      # start new path for next char,
      # move to save location
      "  currentpoint newpath moveto",
      "} def",
      
      if (charpath) {
          c("/show {",
            "  dup length -1 add 0 exch 1 exch {strokechar} for",
            "} def")
      } else {
          c("/show {",
            "  mytext",
            "} def")
      },
      
      "end",
      # end global settings
      "false setglobal",
      # Dummy LOCAL dict for top-level defs (e.g., defs
      # of main dictionary) in file to be run
      "/dummy 100 dict def",
      "%%EndProcSet",
      "%% EndProlog",
      "",
      "convertToR begin",
      "dummy begin",
      "")
}

PScaptureFoot <-
    c(
      # XML file footer info
      "(<summary count=') print convertToR /id get 1 sub str cvs print (') print",
      "( ymax=') print convertToR /ymax get str cvs print (') print",
      "( ymin=') print convertToR /ymin get str cvs print (') print",
      "( xmax=') print convertToR /xmax get str cvs print (') print",
      "( xmin=') print convertToR /xmin get str cvs print (') print",
      "(/>\n\n) print",
      "(</picture>) print",

      # EOF
      "%% EOF"
      )

# Generate RGML file from PostScript file
PostScriptTrace <- function(file, outfilename, charpath=TRUE) {
    # Create temporary PostScript file which loads
    # dictionary redefining stroke and fill operators
    # and then runs target PostScript file
    psfilename <- paste("capture", basename(file), sep="")
    psfile <- file(psfilename, "w")
    writeLines(PScaptureHead(charpath), psfile)
    # Reconstitute file name here to handle Windows-style paths
    # in the file name
    writeLines(paste("(", file.path(dirname(file), basename(file)),
                     ") run", sep=""), psfile)
    writeLines(PScaptureFoot, psfile)
    close(psfile)

    if (missing(outfilename)) {
        outfilename <- paste(basename(file), ".xml", sep="")
    }
    
    # Run temp file using ghostscript
    gscmd <- Sys.getenv("R_GSCMD")
    if (is.null(gscmd) || nchar(gscmd) == 0) {
        gscmd <- switch(.Platform$OS.type,
                        unix = "gs",
                        windows = "gswin32c.exe")
    }
    outfile <- switch(.Platform$OS.type,
                      unix = "/dev/null",
                      windows = tempfile())
    cmd <- paste(gscmd, 
                 " -q -dBATCH -dNOPAUSE -sDEVICE=pswrite -sOutputFile=",
                 outfile, " -sstdout=",
                 outfilename, " ",
                 psfilename, sep="")
    ret <- switch(.Platform$OS.type,
                  unix = system(cmd),
                  windows = system(cmd, invisible = TRUE))
    if(ret != 0)
        stop(gettextf("status %d in running command '%s'", ret, cmd),
             domain = NA)
    invisible(cmd)
}
