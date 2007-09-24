\name{@ANNOBJPREFIX@SMART}
\alias{@ANNOBJPREFIX@SMART}
\title{Map Yeast ORF Identifiers to SMART IDs}
\description{
  @ANNOBJPREFIX@SMART is an R object that provides mappings between yeast
  ORF Identifiers and the associated SMART identifiers.
}
\details{
  Each yeast systematic name maps to a vector of SMART identifiers.

  Mappings were based on data provided by:

  Saccharomyces Genome Database

  Package built on Thu Mar 15 18:04:19 2007
}
\references{
  SMART website: \url{http://smart.embl-heidelberg.de/}
}
\examples{
        # Convert to a list
        xxx <- as.list(@ANNOBJPREFIX@SMART)
        # randomly display 10 probes
        sample(xxx, 10)
}
\keyword{datasets}

