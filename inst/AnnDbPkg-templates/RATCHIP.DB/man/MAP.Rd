\name{@ANNOBJPREFIX@MAP}
\alias{@ANNOBJPREFIX@MAP}
\title{Map between Manufacturer Identifiers and cytogenetic maps/bands}
\description{
   @ANNOBJPREFIX@MAP is an R object that provides mappings between
   manufacturer identifiers and cytoband locations.
}
\details{
  Each manufacturer identifier is mapped to a vector of cytoband locations. The
  vector length may be one or longer, if there are multiple reported
  chromosomal locations for a given gene. An \code{NA} is reported for
  any manufacturer identifiers that cannot be mapped to a cytoband at this time.
  
  Mappings were based on data provided by: @MAPSOURCE@

}
\references{
  \url{http://www.ncbi.nlm.nih.gov}
}
\examples{
        x <- @ANNOBJPREFIX@MAP
        # Get the probe identifiers that are mapped to any cytoband
        mapped_probes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_probes])
        if(length(xx) > 0) {
          # Get the MAP for the first five probes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}

