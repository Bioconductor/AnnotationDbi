\name{@ANNOBJPREFIX@MGI}
\alias{@ANNOBJPREFIX@MGI}
\alias{@ANNOBJPREFIX@MGI2PROBE}
\title{Map MGI gene accession numbers with manufacturer identifiers}  
\description{ 
  @ANNOBJPREFIX@MGI is an R object that contains mappings between 
  manufacturer identifiers and Jackson Laboratory MGI gene accession numbers.    
}
\details{
  This object is a simple mapping of manufacturer identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to MGI
  gene Accession Numbers.

  Mappings were based on data provided by: @MGISOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@MGI
    # Get the manufacturer IDs that are mapped to an MGI ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the MGI IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map MGI2EG:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@MGI2PROBE)
    if(length(xx) > 0){
       # Gets the manufacturer IDs for the first five MGI IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



