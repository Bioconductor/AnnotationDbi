\name{db_file}

\alias{db_file}


\title{Path to the SQLite file containing the package annotation data}

\description{
  Path to the SQLite file containing the package annotation data.
}

\details{
  Informative only.
}

\seealso{
  \code{\link{db_conn}}
}

\examples{
  ## Object 'db_conn' was created with:
  db_conn <- dbConnect(SQLite(), dbname=db_file, cache_size=64000, synchronous=0)
}

\keyword{utilities}
\keyword{datasets}

