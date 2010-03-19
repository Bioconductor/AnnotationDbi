\name{@ANNOBJPREFIX@LOCUSTAG}
\alias{@ANNOBJPREFIX@LOCUSTAG}
\title{Map Entrez Gene IDs to Locus Tag IDs}
\description{
 @ANNOBJPREFIX@LOCUSTAG is an R object that provides mappings between entrez gene
 identifiers and the locus tag identifier associated to the gene of interest. 
}
\details{
  Each entrez gene identifier maps to a locus tag identifier. 
  
  Mappings were based on data provided by: @LOCUSTAGSOURCE@
}

\examples{
        x <- @ANNOBJPREFIX@LOCUSTAG
        # Get the entrez gene identifiers that are mapped to a locus tag
        mapped_genes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_genes])
        if(length(xx) > 0) {
          # Get the LOCUSTAG for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}

