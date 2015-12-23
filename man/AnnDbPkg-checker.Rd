\name{AnnDbPkg-checker}

\alias{AnnDbPkg-checker}

\alias{checkMAPCOUNTS}


\title{Check the SQL data contained in an SQLite-based annotation package}

\description{
  Check the SQL data contained in an SQLite-based annotation package.
}

\usage{
  checkMAPCOUNTS(pkgname)
}

\arguments{
  \item{pkgname}{
    The name of the SQLite-based annotation package to check.
  }
}


\author{H. Pages}

\seealso{
  \code{\link[AnnotationForge:AnnDbPkg-maker]{AnnDbPkg-maker}}
}

\examples{
  checkMAPCOUNTS("org.Sc.sgd.db")
}

\keyword{utilities}

