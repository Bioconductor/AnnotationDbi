\name{@ANNOBJPREFIX@ALIAS2PROBE}
\alias{@ANNOBJPREFIX@ALIAS2PROBE}
\title{Map between Common Gene Symbol Identifiers and Manufacturer Identifiers}
\description{
  @ANNOBJPREFIX@ALIAS is an R object that provides mappings between
  common gene symbol identifiers and manufacturer identifiers.
}
\details{
  Each gene symbol is mapped to a named vector of manufacturer identifiers. The name
  represents the gene symbol and the vector contains all manufacturer identifiers
  that are found for that symbol. An \code{NA} is reported for any gene symbol that 
  cannot be mapped to any manufacturer identifiers.

  Mappings were based on data provided by: @ALIAS2PROBESOURCE@
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@ALIAS2PROBE)
    # Remove pathway identifiers that do not map to any probe id
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
        # The probe identifiers for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }

}

\keyword{datasets}
