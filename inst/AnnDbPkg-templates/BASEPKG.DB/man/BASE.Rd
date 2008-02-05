\name{@ANNOBJPREFIX@.db0}
\alias{@ANNOBJPREFIX@.db0}
\alias{@ANNOBJPREFIX@}
\title{Bioconductor annotation BASE package}
\description{
  Welcome to the @ANNOBJPREFIX@.db0 annotation Package.  This is an
  organism specific package and is ONLY meant as a means for
  distributing the latest Annotation Databases needed by the sqlForge
  code inside of AnnotationDbi to produce normal Annotation Packages.

  Users are warned that the direct calling of the database inside this
  package could be a very bad idea depending on what you are planning to
  do.  This is because the schema for the databases in this package
  COULD CHANGE AT ANY TIME.  Because of this, no schemas are provided.

  For access to more consistent schemas, please use the sqlForge package
  to generate a normal chip or organism based annotation package, or
  else use one of these other annotation packages directly.
}
\examples{
\dontrun{
  # You should never have to use this package for anything directly.
  # Its purpose is only to put the latest databases into a consistent
  # location so that the sqlForge code can build annotation packages from
  # them.  All you should ever have to do is use biocLite to obtain this
  # package and then use the appropriate wrapper functions from
  # AnnotationDbi.  Some examples Include:
  # makeHUMANCHIP_DB(), makeMOUSECHIP_DB(), makeRATCHIP_DB(),
  # makeFLYCHIP_DB(), makeYEASTCHIP_DB(), makeARABIDOPSISCHIP_DB()
}
}
\keyword{datasets}
