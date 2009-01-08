\name{@ANNOBJPREFIX@SGD}
\alias{@ANNOBJPREFIX@SGD}
\alias{@ANNOBJPREFIX@SGD2EG}
\title{Map SGD acession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@SGD is an R object that contains mappings between 
  Entrez Gene identifiers and SGD accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to SGD
  accession numbers.

  Mappings were based on data provided by: @SGDSOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@SGD
    # Get the entrez gene identifiers that are mapped to a SGD ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Sgd IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map SGD2EG:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@SGD2EG)
    if(length(xx) > 0){
       # Gets the entrez gene identifiers for the first five SGD IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



