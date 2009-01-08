\name{@ANNOBJPREFIX@ORF}
\alias{@ANNOBJPREFIX@ORF}
\alias{@ANNOBJPREFIX@ORF2EG}
\title{Map ORF acessions with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@ORF is an R object that contains mappings between 
  Entrez Gene identifiers and ORF accessions.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to ORF
  accessions.

  Mappings were based on data provided by: @ORFSOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@ORF
    # Get the entrez gene identifiers that are mapped to a ORF ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Orf IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map ORF2EG:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ORF2EG)
    if(length(xx) > 0){
       # Gets the entrez gene identifiers for the first five ORF IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



