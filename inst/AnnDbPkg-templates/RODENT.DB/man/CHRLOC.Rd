\name{@ANNOBJPREFIX@CHRLOC}
\alias{@ANNOBJPREFIX@CHRLOC}
\title{Entrez Gene IDs to Chromosomal Location}
\description{
  @ANNOBJPREFIX@CHRLOC is an R object that maps entrez gene identifiers to the
  starting position of the gene. The position of a gene is
  measured as the number of base pairs.
}
\details{
  Each entrez gene identifier maps to a named vector of chromosomal locations,
  where the name indicates the chromosome. 
  
  Chromosomal locations on both the sense and antisense strands are
  measured as the number of base pairs from the p (5' end of the sense
  strand) to q (3' end of the sense strand) arms. Chromosomal locations on the
  antisense strand have a leading "-" sign (e. g. -1234567). 

  Mappings were based on data provided by: @CHRLOCSOURCE@
  
}
\examples{
        x <- @ANNOBJPREFIX@CHRLOC
        # Get the entrez gene identifiers that are mapped to chromosome locations
        mapped_genes <- mappedKeys(x)
        # Convert to a list
        xx <- as.list(x, keys=mapped_genes)
        if(length(xx) > 0) {
          # Get the CHRLOC for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}

