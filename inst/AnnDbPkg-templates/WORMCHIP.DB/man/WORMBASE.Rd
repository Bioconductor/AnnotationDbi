\name{@ANNOBJPREFIX@WORMBASE}
\alias{@ANNOBJPREFIX@WORMBASE}
\alias{@ANNOBJPREFIX@WORMBASE2PROBE}
\title{Map Wormbase accession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@WORMBASE is an R object that contains mappings between 
  Entrez Gene identifiers and Wormbase accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Wormbase
  Accessions.

  Mappings were based on data provided by: @WORMBASESOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@WORMBASE
    # Get the entrez gene IDs that are mapped to an Wormbase ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Wormbase IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
}
\keyword{datasets}



