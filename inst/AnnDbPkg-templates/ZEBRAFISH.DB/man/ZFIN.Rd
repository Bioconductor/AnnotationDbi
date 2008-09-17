\name{@ANNOBJPREFIX@ZFIN}
\alias{@ANNOBJPREFIX@ZFIN}
\title{Map Zfin accession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@ZFIN is an R object that contains mappings between 
  Entrez Gene identifiers and Zfin accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Zfin
  Accession Numbers.

  Mappings were based on data provided by: @ZFINSOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@ZFIN
    # Get the entrez gene IDs that are mapped to a Zfin ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Zfin gene IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
}
\keyword{datasets}



