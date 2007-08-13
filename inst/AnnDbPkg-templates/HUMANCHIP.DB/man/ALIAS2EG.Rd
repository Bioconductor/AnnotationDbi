\name{@ANNOBJPREFIX@ALIAS2PROBE}
\alias{@ANNOBJPREFIX@ALIAS2EG}
\title{Map between Common Gene Symbol Identifiers and Entrez Gene}
\description{
  @ANNOBJPREFIX@ALIAS is an R object that provides mappings between
  common gene symbol identifiers and entrez gene.
}
\details{
  Each gene symbol is mapped to its entrez gene identifier. 
  An \code{NA} is reported for any gene symbols that cannot 
  be mapped to an entrez gene identifier.

  Mappings were based on data provided by: @ALIAS2EGSOURCE@
}
\references{
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene}
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@ALIAS2EG)
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
