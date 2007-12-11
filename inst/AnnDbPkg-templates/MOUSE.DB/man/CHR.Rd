\name{@ANNOBJPREFIX@CHR}
\alias{@ANNOBJPREFIX@CHR}
\title{Map Entrez Gene IDs to Chromosomes}
\description{
 @ANNOBJPREFIX@CHR is an R object that provides mappings between entrez gene
 identifiers and the chromosome that contains the gene of interest. 
}
\details{
  Each entrez gene identifier maps to a vector of a chromosome. 
  
  Mappings were based on data provided by: @CHRSOURCE@
}

\examples{
        x <- @ANNOBJPREFIX@CHR
        # Get the entrez gene identifiers that are mapped to a chromosome
        mapped_genes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_genes])
        if(length(xx) > 0) {
          # Get the CHR for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}

