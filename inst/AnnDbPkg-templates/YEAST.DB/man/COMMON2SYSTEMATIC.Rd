\name{YEASTCOMMON2SYSTEMATIC}
\alias{YEASTCOMMON2SYSTEMATIC}
\title{Map Between Yeast Common Names and Systematic Names}
\description{
  YEASTCOMMON2SYSTEMATIC is an R environment that maps
  the yeast common names (gene names and aliases) to the
  corresponding yeast systematic names.
}
\details{
  Each yeast common name, either gene name or alias, maps to
  a vector of systematic names.  This mapping is the reverse
  mappings of YEASTGENENAME and YEASTALIAS.  
  
  Mappings were based on data provided by:

  Yeast Genome: Not available
  
  Package built Thu Mar 15 18:04:05 2007
}

\examples{
     # Convert to a list
     xx <- as.list(YEASTCOMMON2SYSTEMATIC)
     # Remove probes that do not map to any COMMON2SYSTEMATIC
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the systematic gene names for the first five gene names/alias
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

