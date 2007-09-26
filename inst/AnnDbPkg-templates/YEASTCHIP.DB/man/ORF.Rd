\name{@ANNOBJPREFIX@ORF}
\alias{@ANNOBJPREFIX@ORF}
\title{Map Manufacturer Identifiers to Open Reading Frame (ORF) Identifiers} 
\description{
  @ANNOBJPREFIX@ORF is an R object that provides mappings between
  manufacturer and ORF identifiers.
}
\details{
  Each manufacturer identifier is mapped to a vector of ORF identifiers. The length of
  the vector may be one or longer, depending on how many ORF identifiers the
  manufacturer identifier can be mapped to. An \code{NA} is reported for any
  manufacturer identifier that cannot be mapped to an ORF identifier at this time.
  
  Annotation based on data provided by: @ORFSOURCE@

}
\references{
\url{http://www.yeastgenome.org/DownloadContents.shtml}
}
\examples{
        x <- @ANNOBJPREFIX@ORF
        # Get the probe identifiers that are mapped to ORF identifiers
        mapped_probes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_probes])
        if(length(xx) > 0) {
          # Get the ORF identifiers for the first five probes
          xx[1:5]
          # For the first probe
          xx[[1]]
        }
}
\keyword{datasets}

