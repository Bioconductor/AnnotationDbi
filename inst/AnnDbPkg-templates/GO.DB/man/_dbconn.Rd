\name{@ANNOBJPREFIX@_dbconn}

\alias{@ANNOBJPREFIX@_dbconn}
\alias{@ANNOBJPREFIX@_dbfile}


\title{Returns a connection object to the package annotation DB}

\description{
  Returns a connection object to the package annotation DB.
}

\usage{
  @ANNOBJPREFIX@_dbconn()
}

\arguments{
}

\details{
  Don't call \code{\link[DBI]{dbDisconnect}} on the connection object
  returned by \code{@ANNOBJPREFIX@_dbconn()} or you will
  break all the \code{AnnDbObj} instances defined in this package!

  In addition to the \code{@ANNOBJPREFIX@_dbconn} function,
  the \code{@ANNOBJPREFIX@_dbfile} object is provided: it's a single
  character string containing the path to the file
  containing the package annotation DB.
}

\value{
  \code{@ANNOBJPREFIX@_dbconn()}: a connection object to the package
  annotation DB.

  \code{@ANNOBJPREFIX@_dbfile}: the path to the file containing the package
  annotation DB.
}

\seealso{
  \code{\link[DBI]{dbGetQuery}}
}

\examples{
  ## Count the number of rows in the "go_term" table:
  dbGetQuery(@ANNOBJPREFIX@_dbconn(), "SELECT COUNT(*) FROM go_term")

  ## The connection object returned by @ANNOBJPREFIX@_dbconn() was created with:
  dbConnect(SQLite(), dbname=@ANNOBJPREFIX@_dbfile, cache_size=64000, synchronous=0)
}

\keyword{utilities}
\keyword{datasets}

