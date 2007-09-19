\name{@ANNOBJPREFIX@_dbconn}

\alias{@ANNOBJPREFIX@_dbconn}


\title{Returns the connection object to the package annotation DB}

\description{
  Returns the connection object to the package annotation DB.
}

\details{
  Don't call \code{\link[DBI]{dbDisconnect}} on the connection object
  returned by \code{@ANNOBJPREFIX@_dbconn()} or you will
  break all the \code{AnnDbObj} instances defined in this package!
}

\seealso{
  \code{\link[DBI]{dbGetQuery}},
  \code{\link{@ANNOBJPREFIX@_dbfile}}
}

\examples{
  ## Count the number of rows in the "genes" table:
  dbGetQuery(@ANNOBJPREFIX@_dbconn(), "SELECT COUNT(*) FROM genes")
}

\keyword{utilities}
\keyword{datasets}

