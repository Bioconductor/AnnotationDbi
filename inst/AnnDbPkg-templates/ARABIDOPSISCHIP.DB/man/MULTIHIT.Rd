\name{@ANNOBJPREFIX@MULTIHIT}
\alias{@ANNOBJPREFIX@MULTIHIT}
\title{Map Manufacturer identifiers to GenBank Accession Numbers}  
\description{ 
  @ANNOBJPREFIX@MULTIHIT is a R object that contains mappings between a
  manufacturer's identifiers and GenBank accession numbers.     
}
\details{
  This mapping was produced by first mapping the manufacturer's identifiers to
  Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene}. The
  mapped Entrez Gene identifiers then serve as the point of linkage to the
  GenBank accession numbers.

  Each manufacturer identifier maps to a vector containing a GenBank accession number.

  Annotation based on data provided by: @MULTIHITSOURCE@
}

\examples{
        x <- @ANNOBJPREFIX@MULTIHIT
        # Get the probe identifiers that are mapped to a MULTIHIT
        mapped_probes <- mappedNames(x)
        # Convert to a list
        xx <- as.list(x, names=mapped_probes)
        if(length(xx) > 0) {
          # Get the MULTIHIT for the first five probes
          xx[1:5]
          # For the first probe
          xx[[1]]
        }
    }
}
\keyword{datasets}

