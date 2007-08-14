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

  This mapping includes ALL gene symbols including those which are already listed 
  in the SYMBOL map.  The SYMBOL map is meant to only list official gene symbols, 
  while the ALIAS maps are meant to store all used symbols.

  Mappings were based on data provided by: @ALIAS2PROBESOURCE@
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@ALIAS2PROBE)
    if(length(xx) > 0){
        # Get the probe identifiers for the first two aliases
        xx[1:2]
        # Get the first one
        xx[[1]]
    }

}

\keyword{datasets}
