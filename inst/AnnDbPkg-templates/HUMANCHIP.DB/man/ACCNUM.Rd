\name{@ANNOBJPREFIX@ACCNUM}
\alias{@ANNOBJPREFIX@ACCNUM}
\title{Map Manufacturer identifiers to GenBank Accession Numbers}  
\description{ 
  @ANNOBJPREFIX@ACCNUM is an R object that contains mappings between a
  manufacturer's identifiers and GenBank accession numbers.    
}
\details{
  This object was produced by first mapping the manufacturer's identifiers to
  Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene}. The
  mapped Entrez Gene identifiers then serve as the point of linkage to the
  GenBank accession numbers.

  Each manufacturer identifier maps to a vector containing a GenBank accession number.

  Mappings were based on data provided by: @ACCNUMSOURCE@
}

\examples{
        x <- @ANNOBJPREFIX@ACCNUM
        # Get the probe identifiers that are mapped to an ACCNUM
        mapped_probes <- mappedKeys(x)
        # Convert to a list
        xx <- as.list(x, names=mapped_probes)
        if(length(xx) > 0) {
          # Get the ACCNUM for the first five probes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}

