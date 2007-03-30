\name{makeAnnDbPkg}

\alias{makeAnnDbPkg}
\alias{makeAnnDbPkg,AnnDbPkgSeed-method}
\alias{makeAnnDbPkg,list-method}
\alias{makeAnnDbPkg,character-method}


\title{Creates an SQLite-based annotation package}

\description{
  Creates an SQLite-based annotation package from an SQLite file.
}

\usage{
  makeAnnDbPkg(x, db_file, dest_dir=".")
}

\arguments{
  \item{x}{
    A AnnDbPkgSeed object, a list, a string or a regular expression.
  }
  \item{db_file}{
    The path to the SQLite containing the annotation data for the package
    to be built.
  }
  \item{dest_dir}{
    The directory where the package will be created.
  }
}

\details{
}

\seealso{
  \link{AnnDbPkgSeed-class}
}

\examples{
  ## With a "AnnDbPkgSeed" object:
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
  #makeAnnDbPkg(seed, "path/to/hgu133a2.sqlite")

  ## With a package name:
  ## (Note that in this case makeAnnDbPkg() will use the package description
  ## found in the central index file ANNDBPKG-INDEX.TXT located in the
  ## AnnotationDbi package.)
  #makeAnnDbPkg("hgu133a2db")

  ## Regular expressions are accepted e.g. to make all the packages described
  ## in the central index:
  #makeAnnDbPkg(".*")
}

\keyword{methods}

