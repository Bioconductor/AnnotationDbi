\name{@ANNOBJPREFIX@ALIAS2ORF}
\alias{@ANNOBJPREFIX@ALIAS2ORF}
\title{Map between Common Gene Symbol Identifiers and ORF}
\description{
  @ANNOBJPREFIX@ALIAS is an R object that provides mappings between
  common gene symbol identifiers and ORF identifiers.
}
\details{
  Each gene symbol maps to a named vector containing the corresponding ORF 
  identifier. The name of the vector corresponds to the gene symbol.  Since gene 
  symbols are sometimes redundantly assigned in the literature, users are cautioned 
  that this map may produce multiple matching results for a single gene symbol.  Users 
  should map back from the ORF IDs produced to determine which result is the 
  one they want when this happens.

  Because of this problem with  redundant assigment of gene symbols, is it never 
  advisable to use gene symbols as primary identifiers.  

  This mapping includes ALL gene symbols including those which are already listed 
  in the SYMBOL map.  The SYMBOL map is meant to only list official gene symbols, 
  while the ALIAS maps are meant to store all used symbols.

  Mappings were based on data provided by: @ALIAS2ORFSOURCE@
}
\references{
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene}
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@ALIAS2ORF)
    # Remove pathway identifiers that do not map to any ORF id
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
        # The ORF identifiers for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }

}

\keyword{datasets}
