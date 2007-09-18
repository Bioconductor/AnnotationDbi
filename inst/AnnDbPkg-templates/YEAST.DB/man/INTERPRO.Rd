\name{YEASTINTERPRO}
\alias{YEASTINTERPRO}
\title{Map Yeast Systematic Names to InterPro IDs}
\description{
  YEASTINTERPRO is an R environment that provides mappings between yeast
  systematic names and the associated InterPro identifiers.
}
\details{
  Each yeast systematic name maps to a vector of InterPro identifiers.

  Mappings were based on data provided by:

  Saccharomyces Genome Database

  Package built on Thu Mar 15 18:04:19 2007
}
\references{
  InterPro website: \url{http://www.ebi.ac.uk/interpro/}
  and
  Data source: \url{http://gopher6/compbio/annotationSourceData/ftp.yeastgenome.org/pub/yeast/sequence_similarity/domains/domains.tab}
}
\examples{
        # Convert to a list
        xxx <- as.list(YEASTINTERPRO)
        # randomly display 10 probes
        sample(xxx, 10)
}
\keyword{datasets}

