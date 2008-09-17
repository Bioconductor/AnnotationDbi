\name{@ANNOBJPREFIX@ENSEMBL}
\alias{@ANNOBJPREFIX@ENSEMBL}
\alias{@ANNOBJPREFIX@ENSEMBL2PROBE}
\title{Map Ensembl gene accession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@ENSEMBL is an R object that contains mappings between 
  Entrez Gene identifiers and Ensembl gene accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Ensembl
  gene Accession Numbers.

  Mappings were based on data provided by: @ENSEMBLSOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@ENSEMBL
    # Get the entrez gene IDs that are mapped to an Ensembl ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Ensembl gene IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map ENSEMBL2PROBE:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ENSEMBL2PROBE)
    if(length(xx) > 0){
       # Gets the entrez gene IDs for the first five Ensembl IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



