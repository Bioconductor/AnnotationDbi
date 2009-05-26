\name{@ANNOBJPREFIX@ARACYC}
\alias{@ANNOBJPREFIX@ARACYC}
\title{Mappings between TAIR identifiers and KEGG pathway identifiers}
\description{
  AraCyc \url{http://www.arabidopsis.org/tools/aracyc/} maintains pathway data
  for Arabidopsis thaliana. @ANNOBJPREFIX@ARACYC maps TAIR identifiers to the
  common names of the pathways in which the genes represented
  by the tair identifiers are involved. Information is obtained from
  AraCyc.
}
\details{
  Annotation based on data provided by: @ARACYCSOURCE@
}
\references{
  \url{http://www.genome.ad.jp/kegg/}
}

\examples{
  x <- @ANNOBJPREFIX@ARACYC
  # Get the tair identifiers that are mapped to pathways
  mapped_tairs <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_tairs])
  if(length(xx) > 0) {
    # Get the pathways for the first five tairs
    xx[1:5]
    # For the first tair
    xx[[1]]
  }
}

\keyword{datasets}

