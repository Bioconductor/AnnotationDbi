\name{@ANNOBJPREFIX@FLYBASE}
\alias{@ANNOBJPREFIX@FLYBASE}
\alias{@ANNOBJPREFIX@FLYBASE2EG}
\title{Map FlyBase acession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@FLYBASE is an R object that contains mappings between 
  Entrez Gene identifiers and FlyBase accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Flybase
  accession numbers.

  Mappings were based on data provided by: @FLYBASESOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@FLYBASE
    # Get the entrez gene identifiers that are mapped to a Flybase ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the FlyBase IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map FLYBASE2EG:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@FLYBASE2EG)
    if(length(xx) > 0){
       # Gets the entrez gene identifiers for the first five FlyBase IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



