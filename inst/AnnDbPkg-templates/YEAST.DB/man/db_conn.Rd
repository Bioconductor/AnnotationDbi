\name{db_conn}

\alias{db_conn}


\title{Connection object to the package annotation DB}

\description{
  Connection object to the package annotation DB.
}

\details{
  Don't call \code{\link[DBI]{dbDisconnect}} on \code{db_conn} or you will
  break all the \code{AnnDbObj} instances defined in this package!
}

\seealso{
  \code{\link[DBI]{dbGetQuery}},
  \code{\link{db_file}}
}

\examples{
  ## Count the number of rows in the "sgd" table:
  dbGetQuery(db_conn, "SELECT COUNT(*) FROM sgd")
}

\keyword{utilities}
\keyword{datasets}

