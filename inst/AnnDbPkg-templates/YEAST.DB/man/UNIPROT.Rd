\name{@ANNOBJPREFIX@UNIPROT}
\alias{@ANNOBJPREFIX@UNIPROT}
\title{Map Uniprot accession numbers with Systematic ORF identifiers}  
\description{ 
  @ANNOBJPREFIX@UNIPROT is an R object that contains mappings between 
  Systematic ORF identifiers and Uniprot accession numbers.    
}
\details{
  This object is a simple mapping of Systematic ORF identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Uniprot
  Accession Numbers.

  Mappings were based on data provided by: @UNIPROTSOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@UNIPROT
    # Get the Systematic ORF IDs that are mapped to a Uniprot ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Uniprot gene IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
}
\keyword{datasets}



