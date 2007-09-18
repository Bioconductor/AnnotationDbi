\name{@ANNOBJPREFIX@COMMON2SYSTEMATIC}
\alias{@ANNOBJPREFIX@COMMON2SYSTEMATIC}
\title{Map Between Yeast Common Names and Systematic Names}
\description{
  @ANNOBJPREFIX@COMMON2SYSTEMATIC is an R environment that maps
  the yeast common names (gene names and aliases) to the
  corresponding yeast systematic names.
}
\details{
  Each yeast common name, either gene name or alias, maps to
  a vector of systematic names.  This mapping is the reverse
  mappings of @ANNOBJPREFIX@GENENAME.  
  
  Mappings were based on data provided by:

  @COMMON2SYSTEMATICSOURCE@
}

\examples{
     # Convert to a list
     xx <- as.list(@ANNOBJPREFIX@COMMON2SYSTEMATIC)
     # Remove probes that do not map in COMMON2SYSTEMATIC
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the systematic gene names for the first five gene names/alias
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

