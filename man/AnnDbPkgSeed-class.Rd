\name{AnnDbPkgSeed-class}
\docType{class}

\alias{class:AnnDbPkgSeed}
\alias{AnnDbPkgSeed-class}


\title{The AnnDbPkgSeed class}

\description{
  \code{AnnDbPkgSeed} instances are intended to be passed to
  the \code{\link{makeAnnDbPkg}} generic function in the context
  of the automated mass production of SQLite-based annotation packages.
}

\seealso{
  \code{\link{makeAnnDbPkg}}
}

\examples{
  seed <- new("AnnDbPkgSeed",
      Package="hgu133a2db",
      Version="0.0.99",
      PkgTemplate="HGU95AV2DB",
      DBschema="HGU95AV2_DB",
      AnnObjPrefix="hgu133a2",
      AnnObjTarget="chip hgu133a2",
      organism="Homo sapiens",
      species="Human",
      manufacturer="Affymetrix",
      chipName="Human Genome U133A 2.0 Array",
      manufacturerUrl="http://www.affymetrix.com/support/technical/byproduct.affx?product=hgu133-20"
  )

  ## Then use this "seed" together with the SQLite file containing the
  ## annotation data for the hgu133a2 chip to generate the hgu133a2db package:
  #makeAnnDbPkg(seed, "path/to/hgu133a2.sqlite")
}

\keyword{classes}

