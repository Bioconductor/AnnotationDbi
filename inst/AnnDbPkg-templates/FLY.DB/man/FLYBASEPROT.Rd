\name{@ANNOBJPREFIX@FLYBASEPROT}
\alias{@ANNOBJPREFIX@FLYBASEPROT}
\alias{@ANNOBJPREFIX@FLYBASEPROT2EG}
\title{Map FlyBase Protein Acession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@FLYBASE is an R object that contains mappings between 
  Entrez Gene identifiers and FlyBase protein accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Flybase
  protein accession numbers.

  Mappings were based on data provided by: @FLYBASESOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@FLYBASEPROT
    # Get the entrez gene IDs that are mapped to a Flybase prot ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the FlyBase protein IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map FLYBASEPROT2EG:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@FLYBASEPROT2EG)
    if(length(xx) > 0){
       # Gets the entrez gene IDs for the first five FlyBase protein IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



