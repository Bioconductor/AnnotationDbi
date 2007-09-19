\name{@ANNOBJPREFIX@_dbfile}

\alias{@ANNOBJPREFIX@_dbfile}


\title{Path to the SQLite file containing the package annotation data}

\description{
  Path to the SQLite file containing the package annotation data.
}

\details{
  Informative only.
}

\seealso{
  \code{\link{@ANNOBJPREFIX@_dbconn}}
}

\examples{
  ## The connection object returned by @ANNOBJPREFIX@_dbconn() was created with:
  dbConnect(SQLite(), dbname=@ANNOBJPREFIX@_dbfile, cache_size=64000, synchronous=0)
}

\keyword{utilities}
\keyword{datasets}

