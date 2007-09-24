\name{@ANNOBJPREFIX@COMMON2ORF}
\alias{@ANNOBJPREFIX@COMMON2ORF}
\title{Map Between Yeast Common Names and ORF Identifiers}
\description{
  @ANNOBJPREFIX@COMMON2ORF is an R object that maps
  the yeast common names (gene names and aliases) to the
  corresponding yeast ORF identifiers.
}
\details{
  Each yeast common name, either gene name or alias, maps to
  a vector of ORF identifiers.  This mapping is the reverse
  mappings of @ANNOBJPREFIX@GENENAME.  
  
  Mappings were based on data provided by:

  @COMMON2ORFSOURCE@
}

\examples{
     # Convert to a list
     xx <- as.list(@ANNOBJPREFIX@COMMON2ORF)
     # Remove probes that do not map in COMMON2ORF
     xx <- xx[!is.na(xx)]
     if(length(xx) > 0){
         # Gets the ORF identifiers for the first five gene names/alias
         xx[1:5]
         # Get the first one
         xx[[1]]
     }
}
\keyword{datasets}

