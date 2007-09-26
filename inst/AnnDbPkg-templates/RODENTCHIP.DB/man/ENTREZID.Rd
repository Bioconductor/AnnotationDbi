\name{@ANNOBJPREFIX@ENTREZID}
\alias{@ANNOBJPREFIX@ENTREZID}
\alias{@ANNOBJPREFIX@LOCUSID}
\title{Map between Manufacturer Identifiers and Entrez Gene}
\description{
  @ANNOBJPREFIX@ENTREZID is an R object that provides mappings between
  manufacturer identifiers and Entrez Gene identifiers.
}
\details{
  Each manufacturer identifier is mapped to a vector of Entrez Gene identifiers. An
  \code{NA} is assigned to those manufacturer identifiers that can not be mapped
  to an Entrez Gene identifier at this time.
  
  If a given manufacturer identifier can be mapped to different Entrez Gene identifiers
  from various sources, we attempt to select the common identifiers. If a
  concensus cannot be determined, we select the smallest identifier.
  
  Mappings were based on data provided by: @ENTREZIDSOURCE@
  
}
\references{
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene}
}
\examples{
        x <- @ANNOBJPREFIX@ENTREZID
        # Get the probe identifiers that are mapped to an ENTREZ Gene ID
        mapped_probes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_probes])
        if(length(xx) > 0) {
          # Get the ENTREZID for the first five probes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}

\keyword{datasets}

