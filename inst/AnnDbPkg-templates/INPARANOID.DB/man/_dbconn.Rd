
\name{@ANNOBJPREFIX@_dbconn}

\alias{@ANNOBJPREFIX@_dbconn}
\alias{@ANNOBJPREFIX@_dbfile}
\alias{@ANNOBJPREFIX@_dbschema}
\alias{@ANNOBJPREFIX@_dbInfo}


\title{Collect information about the package annotation DB}

\description{
  Some convenience functions for getting a connection object to (or collecting
  information about) the package annotation DB.
}

\usage{
  @ANNOBJPREFIX@_dbconn()
  @ANNOBJPREFIX@_dbfile()
  @ANNOBJPREFIX@_dbschema(file="", show.indices=FALSE)
  @ANNOBJPREFIX@_dbInfo()
}

\arguments{
  \item{file}{
    A connection, or a character string naming the file to print to (see
    the \code{file} argument of the \code{\link[base]{cat}} function for
    the details).
  }
  \item{show.indices}{
    The CREATE INDEX statements are not shown by default.
    Use \code{show.indices=TRUE} to get them.
  }
}

\details{
  \code{@ANNOBJPREFIX@_dbconn} returns a connection object to the
  package annotation DB.  IMPORTANT: Don't call
  \code{\link[DBI:dbConnect]{dbDisconnect}} on the connection object
  returned by \code{@ANNOBJPREFIX@_dbconn} or you will break all the
  \code{\link[AnnotationDbi:AnnDbObj-class]{AnnDbObj}} objects defined
  in this package!

  \code{@ANNOBJPREFIX@_dbfile} returns the path (character string) to the
  package annotation DB (this is an SQLite file).

  \code{@ANNOBJPREFIX@_dbschema} prints the schema definition of the
  package annotation DB.

  \code{@ANNOBJPREFIX@_dbInfo} prints other information about the package
  annotation DB.
}

\value{
  \code{@ANNOBJPREFIX@_dbconn}: a DBIConnection object representing an
  open connection to the package annotation DB.

  \code{@ANNOBJPREFIX@_dbfile}: a character string with the path to the
  package annotation DB.

  \code{@ANNOBJPREFIX@_dbschema}: none (invisible \code{NULL}).

  \code{@ANNOBJPREFIX@_dbInfo}: none (invisible \code{NULL}).
}

\seealso{
  \code{\link[DBI:dbSendQuery]{dbGetQuery}},
  \code{\link[DBI:dbConnect]{dbConnect}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbconn}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbfile}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbschema}},
  \code{\link[AnnotationDbi:AnnDbObj-class]{dbInfo}}
}

\examples{
  ## Count the number of rows in the "metadata" table:
  dbGetQuery(@ANNOBJPREFIX@_dbconn(), "SELECT COUNT(*) FROM metadata")

  ## The connection object returned by @ANNOBJPREFIX@_dbconn() was
  ## created with:
  dbConnect(SQLite(), dbname=@ANNOBJPREFIX@_dbfile(), cache_size=64000,
  synchronous=0)

  @ANNOBJPREFIX@_dbschema()

  @ANNOBJPREFIX@_dbInfo()
}

\keyword{utilities}
\keyword{datasets}

