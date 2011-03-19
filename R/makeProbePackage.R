print.probetable = function(x, maxrows = 3, ...) {
  nr = nrow(x)
  np = min(nr, maxrows)
  cat("Object of class", class(x), "with", nr, "rows and", ncol(x), "columns.\n")
  if(nr>maxrows) cat("First", np, "rows are:\n")
  print.data.frame(x[seq_len(np), ])
}


## ----------------------------------------------------------------------
## The table pt contains a probe to probe-set mapping (many-to-one).
## The CDF environment contains a probe-set to probe mapping (one-to-many).
## Here, we check whether they agree.
## In addition, it uses the information in the CDF to guess
## sizex, the size of the chip in x-direction.
## This is done using the fact that with current-day Affymetrix
## for each PM probe at (x,y) there is a MM probe at (x,y+1).
## (C) Laurent Gautier, Wolfgang Huber 2003
## ----------------------------------------------------------------------
.lgExtraParanoia = function (pt, cdfname) {
  do.call(library, list(cdfname))

  thecdf <- as.environment(paste("package", cdfname, sep=":"))[[cdfname]]
    
  ## Unroll CDF in order to invert the mapping from probe-set -> probe
  ## to probe -> probe-set. psnm1[i] is the probe set name for the i-th probe
  probesetnames = ls(thecdf)
  pm1   = unlist(lapply(probesetnames,
    function(ps) {thecdf[[ps]][,1]}))
  mm1   = unlist(lapply(probesetnames,
    function(ps) {thecdf[[ps]][,2]}))
  psnm1 = unlist(lapply(probesetnames,
    function(ps) {rep(ps, nrow(thecdf[[ps]]))}))

  ## On most chips, PM and MM probe are next to each other on the chip, at same
  ## x coordinate and at adjacent y coordinates. Then, "sizex" is always the same,
  ## namely the size of the chip in x-direction. On some chips, there are few
  ## exceptions.
  tab = table(mm1-pm1)
  sizex = as.numeric(names(tab))[ max(tab)==tab ]

  ## The probe indices according to pt
  pm2   =  pt$y    * sizex + pt$x + 1
  mm2   = (pt$y+1) * sizex + pt$x + 1
  psnm2 = pt[["Probe.Set.Name"]]

  ## Check if the probe set names that are associated with each probe
  ## are the same in both CDF and pt
  z1 = z2 = rep(NA, max(pm1, mm1, pm2, mm2))
  z1[pm1] = z1[mm1] = psnm1
  z2[pm2] = z2[mm2] = psnm2

  diffprob = which(z1 != z2)
  if(length(diffprob)>0) {
    cat("***************************************************************************\n",
        "Found different probe set names in 'CDF package' and 'probe package' for\n",
         length(diffprob), "probes.\n")
    for (i in 1:min(10, length(diffprob)))
      cat(z1[diffprob[i]], z2[diffprob[i]], "\n")
    cat("If you consider this mismatch insignificant, you may want to rerun this\n",
        "function with 'comparewithcdf  = FALSE'. Otherwise, you'll need to\n",
        "figure out the reason for this!\n")
    stop("Stopped")
  }

  invisible(TRUE)
}


## A function that reads tab-delimited probe sequence
## (and other stuff) files from Affymetrix
getProbeDataAffy <- function(arraytype, datafile,
                             pkgname = NULL, comparewithcdf = TRUE)
{
  require(affy) || stop("Could not load library affy.")

  if(missing(datafile)) {
    datafile <- paste(arraytype, "_probe_tab", sep="")
  } else {
    if (is(datafile, "character")) {
      datafile <- file(datafile, "r")
      on.exit(close(datafile))
    }
  }

  arraytype = cleancdfname(arraytype, addcdf=FALSE)
  cdfname   = cleancdfname(arraytype)
  if (is.null(pkgname))
    pkgname = paste(arraytype, "probe", sep="")
  int <- integer(0)
  chr <- ""
  what = list(chr, int, int, int, chr, chr)
  ## If datafile is a connection, scan is like readLines(), so we don't want to skip a line.
  if(inherits(datafile, "connection")){
      head <- scan(datafile, sep="\t", quiet=TRUE, multi.line = FALSE, nlines=1, what="character")
      dat  <- scan(datafile, sep="\t", quiet=TRUE, multi.line = FALSE, what=what)
  }else{
      head <- scan(datafile, sep="\t", quiet=TRUE, multi.line = FALSE, nlines=1, what="character")
      dat  <- scan(datafile, sep="\t", quiet=TRUE, multi.line = FALSE, what=what, skip=1)
  }

  if((any(unlist(head) != c("Probe Set Name", "Probe X", "Probe Y",
     "Probe Interrogation Position", "Probe Sequence", "Target Strandedness")))
     &&
     (any(unlist(head) != c("Probe Set ID", "probe x", "probe y",
     "probe interrogation position", "probe sequence", "target strandedness")))
     ) {
      mess = paste("The data file", datafile, "does not have the expected column names",
         "in its header line. Please make sure it is the right data file. If you are",
         "positive, you may need to write a customized data import function",
         "to replace 'getProbeDataAffy'. You may use 'getProbeDataAffy' as a template.",
         "Please see the help files for the functions 'getProbeDataAffy' and",
         "'makeProbePackage', and the makeProbePackage vignette in package AnnotationDbi.\n")
      stop(mess)
    }

  for (i in which(sapply(what, class)=="numeric")) {
    z = which(is.na(dat[[i]]))
    if(length(z)>0)
      stop(paste("Corrupted data file: found non-number in line ", z[1],
                 " of column ", head[i], ": ", dat[z[1], i]), sep="") 
  }

  ## data frame with the probe data
  pt = data.frame(sequence = I(dat[[5]]),           ## character
                  x        = dat[[2]],  ## integer
                  y        = dat[[3]],  ## integer
                  Probe.Set.Name               = I(dat[[1]]),          ## character 
                  Probe.Interrogation.Position = dat[[4]], ## integer
                  Target.Strandedness          = dat[[6]])             ## factor
  class(pt) = c("probetable", class(pt))

  ## assign
  dataEnv = new.env(parent=emptyenv())
  assign(pkgname, pt, envir=dataEnv)

  datasource = "The probe sequence data was obtained from http://www.affymetrix.com."
  if(is.character(datafile))
    datasource = paste(datasource, " The file name was ", gsub("_", "\\\\_", datafile),
                       ".", sep="")

  symVal = list(ARRAYTYPE  = arraytype,
    DATASOURCE = datasource,
    NROW       = as.character(nrow(pt)),
    NCOL       = as.character(ncol(pt)))

  if(comparewithcdf) .lgExtraParanoia(pt, cdfname)

  return(list(pkgname = pkgname, symVal = symVal, dataEnv = dataEnv))
}


getProbeData_1lq <- function(arraytype, datafile, pkgname = NULL)
{
  if (is.null(pkgname))
    pkgname = paste(arraytype, "probe", sep="")

  ## X        Y          SEQUENCE  DESTYPE
  ## FEATURE  QUALIFIER  EXPOS     PLEN
  ## POS      CBASE      PBASE     TBASE        
  ## IPBASE   UNIT       BLOCK     ATOM
  int = as.integer(0)
  chr = as.character("")
  what=list(int, int, chr, chr,
            chr, chr, int, int,
            int, chr, chr, chr,
            chr, int, int, int)
  toFactor = c(0,0,0,1,
               1,1,0,0,
               0,1,1,1,
               1,0,0,0)
  use = c(1:4, 6:12, 14, 16)

  hd  = scan(datafile, sep="\t", quiet=TRUE, multi.line = FALSE, skip=2, what="character", nlines=1)
  dat = scan(datafile, sep="\t", quiet=TRUE, multi.line = FALSE, skip=3,  what=what, na.strings="!")

  nr = unique(listLen(dat))
  stopifnot(length(nr)==1, length(dat)==length(hd))
  cat(length(hd),"columns and", nr, "rows.\n")

  hd = tolower(hd)
  for(i in which(toFactor!=0))
    dat[[i]] = factor(dat[[i]])

  ## assign
  dataEnv = new.env(parent=emptyenv())
  assign(pkgname, new.env(parent=emptyenv()), envir=dataEnv)
  for(i in use)
    assign(hd[i], dat[[i]], envir=get(pkgname, envir=dataEnv))

  datasource = "The probe sequence data was obtained from \\\\url{http://www.affymetrix.com}."
  if(is.character(datafile))
    datasource = paste(datasource, " The file name was \\\\code{", datafile, "}.", sep="")

  symVal = list(ARRAYTYPE  = arraytype,
    DATASOURCE = datasource,
    NROW       = as.character(nr),
    NCOL       = as.character(length(hd)))

  return(list(pkgname = pkgname, symVal = symVal, dataEnv = dataEnv))
}


##----------------------------------------------------------------------
## Copyright R. Gentleman and W. Huber, 2003, all rights reserved
##----------------------------------------------------------------------
makeProbePackage <- function(arraytype,
                             importfun = "getProbeDataAffy",
                             maintainer,
                             version,
                             species,
                             pkgname = NULL,
                             outdir  = ".",
                             force = FALSE, quiet = FALSE, check = TRUE, build = TRUE, unlink = TRUE, ...)
{
  ## Bureucracy: check arguments
  if (missing(maintainer) || !is.character(maintainer))
    stop(paste("'maintainer' is missing or invalid. Please specify the maintainer of the",
               "package that you want to create in the form: Your name <you@domain>", sep="\n"))
  if (missing(version) || !is.character(version))
    stop(paste("'version' is missing or invalid. Please select a version number larger",
               "than those used for any previous versions of this package.", sep="\n"))
  if(!exists(importfun) || !is.function(get(importfun)))
    stop("'importfun' must be a function.")
  if (missing(species) || !is.character(species))
    stop(paste("'species' is missing or invalid. Please specify the species that the",
               "package will pertain to using the form: Genus_species (e.g., Homo_sapiens).", sep = "\n"))

  ## Call the import function
  ## importRes is a list with three elements:
  ## $pkgname : package name
  ## $dataEnv : environment containing data objects
  ## $symVal  : named list with symbol-value substitutions
  if (!quiet) cat("Importing the data.\n")
  importRes <- do.call(importfun, c(arraytype = arraytype, pkgname = pkgname, list(...)))

  pkgname <- importRes$pkgname
  thispkg <- "AnnotationDbi"
  desc    <- packageDescription(thispkg)

  stopifnot(desc$Package ==thispkg)
  thispkgVers <- desc$Version
  
  symbolValues <- c(importRes$symVal,
                    list(
                         VERSION            = version,
                         CREATOR            = paste("package", thispkg, "version", thispkgVers),
                         ANNOTATIONDBIVERSION = thispkgVers,                                      
                         MAINTAINER         = maintainer,
                         SPECIES            = species))

  ## Create package
  createRes <- createPackage(pkgname,
                             destinationDir = outdir,
                             originDir = system.file("ProbePkg-template", package=thispkg),
                             symbolValues = symbolValues,
                             unlink = unlink, quiet = quiet)

  ## Write the data objects
  if (!quiet) cat("Writing the data.\n")
  save(list  = ls(importRes$dataEnv),
       file  = file.path(createRes$pkgdir, "data", paste(pkgname, ".rda", sep="")),
       envir = importRes$dataEnv,
       compress = TRUE)

  R_exe <- file.path(R.home(), "bin", "R")
  ## R CMD check
  cdir <- getwd()
  setwd(outdir)
  on.exit(setwd(cdir))
  if (check) {
    if (!quiet)
      cat("Checking the package.\n")
    ## Capture output to avoid spewing on screen, then read from log
    checkOut <- system(paste(R_exe, "CMD check", pkgname), intern=TRUE)
    logFile <- file.path(paste(pkgname, "Rcheck", sep="."), "00check.log")
    if (!file.exists(logFile)) {
      stop(paste("Expected but did not find the log-file", logFile, "after R CMD check"))
    } else {
      thelines <- readLines(logFile)
      warns <- grep("WARNING", thelines, value=TRUE)
      errs  <- grep("ERROR", thelines, value=TRUE)
      if (length(warns)>0)
        cat("*** WARNINGS ***\n", warns)
      if (length(errs)>0)
        stop(errs)
    }
    if (unlink)
      unlink(paste(pkgname, ".Rcheck", sep=""), recursive = TRUE)
  }

  ## R CMD build
  if (build) {
    if (!quiet)
      cat("Building the package.\n")
    buildOut <- system(paste(R_exe, "CMD build", ifelse(force, "-force", ""),
                             pkgname), intern=TRUE)
  }
  setwd(cdir)
  return(pkgname)
}

