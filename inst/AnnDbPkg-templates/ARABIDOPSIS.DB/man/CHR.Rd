\name{@ANNOBJPREFIX@CHR}
\alias{@ANNOBJPREFIX@CHR}
\title{Map TAIR IDs to Chromosomes}
\description{
 @ANNOBJPREFIX@CHR is an R object that provides mappings between a
 TAIR identifier and the chromosome that contains the gene of interest. 
}
\details{
  Each TAIR identifier maps to a vector of chromosomes. Due to
  inconsistencies that may exist at the time the object was built,
  the vector may contain more than one chromosome (e.g., the identifier may map
  to more than one chromosome). If the chromosomal location is unknown,
  the vector will contain an \code{NA}.
  
  Mappings were based on data provided by: @CHRSOURCE@
  
}

\examples{
        x <- @ANNOBJPREFIX@CHR
        # Get the tair identifiers that are mapped to a chromosome
        mapped_tairs <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_tairs])
        if(length(xx) > 0) {
          # Get the CHR for the first five tairs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}

