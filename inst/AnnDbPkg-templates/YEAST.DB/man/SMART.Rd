\name{YEASTSMART}
\alias{YEASTSMART}
\title{Map Yeast Systematic Names to SMART IDs}
\description{
  YEASTSMART is an R environment that provides mappings between yeast
  systematic names and the associated SMART identifiers.
}
\details{
  Each yeast systematic name maps to a vector of SMART identifiers.

  Mappings were based on data provided by:

  Saccharomyces Genome Database

  Package built on Thu Mar 15 18:04:19 2007
}
\references{
  SMART website: \url{http://smart.embl-heidelberg.de/}
  and
  Data source: \url{http://gopher6/compbio/annotationSourceData/ftp.yeastgenome.org/pub/yeast/sequence_similarity/domains/domains.tab}
}
\examples{
        # Convert to a list
        xxx <- as.list(YEASTSMART)
        # randomly display 10 probes
        sample(xxx, 10)
}
\keyword{datasets}

