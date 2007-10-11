\name{@ANNOBJPREFIX@FLYBASE}
\alias{@ANNOBJPREFIX@FLYBASE}
\alias{@ANNOBJPREFIX@FLYBASE2PROBE}
\title{Map Entrez Gene identifiers with FlyBase Acession numbers}  
\description{ 
  @ANNOBJPREFIX@FLYBASE is an R object that contains mappings between 
  Entrez Gene identifiers and FlyBase accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Flybase
  Accession Numbers.

  Mappings were based on data provided by: @FLYBASESOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@FLYBASE
    # Get the entrez gene identifiers that are mapped to a FLYBASE ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the FlyBase IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    # For the reverse map:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@FLYBASE2PROBE)
   if(length(xx) > 0){
       # Get the probe identifiers for the first five IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }

}
\keyword{datasets}



