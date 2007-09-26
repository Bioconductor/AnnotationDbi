\name{@ANNOBJPREFIX@ARACYC}
\alias{@ANNOBJPREFIX@ARACYC}
\title{Mappings between probe identifiers and KEGG pathway identifiers}
\description{
  AraCyc \url{http://www.arabidopsis.org/tools/aracyc/} maintains pathway data
  for Arabidopsis thaliana. @ANNOBJPREFIX@ARACYC maps probe identifiers to the
  common names of the pathways in which the genes represented
  by the probe identifiers are involved. Information is obtained from
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
  # Get the probe identifiers that are mapped to pathways
  mapped_probes <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_probes])
  if(length(xx) > 0) {
    # Get the pathways for the first five probes
    xx[1:5]
    # For the first probe
    xx[[1]]
  }
}

\keyword{datasets}

