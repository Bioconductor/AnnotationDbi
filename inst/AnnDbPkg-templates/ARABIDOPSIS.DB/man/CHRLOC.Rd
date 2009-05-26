\name{@ANNOBJPREFIX@CHRLOC}
\alias{@ANNOBJPREFIX@CHRLOC}
\alias{@ANNOBJPREFIX@CHRLOCEND}
\title{Map Manufacturer IDs to Chromosomal Location}
\description{
  @ANNOBJPREFIX@CHRLOC is an R object that maps manufacturer identifiers to the
  starting position of the gene. The position of a gene is
  measured as the number of base pairs.

  The CHRLOCEND mapping is the same as the CHRLOC mapping except that it
  specifies the ending base of a gene instead of the start.
}
\details{
  Each manufacturer identifier maps to a named vector of chromosomal locations,
  where the name indicates the chromosome. Due to inconsistencies that
  may exist at the time the object was built, these vectors may
  contain more than one chromosome and/or location. If the chromosomal
  location is unknown, the vector will contain an \code{NA}.
  
  Chromosomal locations on both the sense and antisense strands are
  measured as the number of base pairs from the p (5' end of the sense
  strand) to q (3' end of the sense strand) arms. Chromosomal locations on the
  antisense strand have a leading "-" sign (e. g. -1234567). 

  Mappings were based on data provided by: @CHRLOCSOURCE@
  
}
\examples{
        x <- @ANNOBJPREFIX@CHRLOC
        # Get the tair identifiers that are mapped to chromosome locations
        mapped_tairs <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_tairs])
        if(length(xx) > 0) {
          # Get the CHRLOC for the first five tairs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}

