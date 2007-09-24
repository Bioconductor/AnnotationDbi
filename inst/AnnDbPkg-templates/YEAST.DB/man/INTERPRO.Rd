\name{@ANNOBJPREFIX@INTERPRO}
\alias{@ANNOBJPREFIX@INTERPRO}
\title{Map Yeast Systematic Names to InterPro IDs}
\description{
  @ANNOBJPREFIX@INTERPRO is an R object that provides mappings between yeast
  ORF identifiers and the associated InterPro identifiers.
}
\details{
  Each yeast ORF identifier maps to a vector of InterPro identifiers.

  Mappings were based on data provided by:

  @INTERPROSOURCE@
}
\references{
  InterPro website: \url{http://www.ebi.ac.uk/interpro/}
}
\examples{
        # Convert to a list
        xxx <- as.list(@ANNOBJPREFIX@INTERPRO)
        # randomly display 10 probes
        sample(xxx, 10)
}
\keyword{datasets}

