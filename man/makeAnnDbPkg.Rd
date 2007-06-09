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
      Package="hgu133a2.db",
      Version="0.0.99",
      PkgTemplate="HGU95AV2DB",
      AnnObjPrefix="hgu133a2"
  )
  #makeAnnDbPkg(seed, "path/to/hgu133a2.sqlite")

  ## With package names:
  ## (Note that in this case makeAnnDbPkg() will use the package descriptions
  ## found in the master index file ANNDBPKG-INDEX.TXT located in the
  ## AnnotationDbi package.)
  #makeAnnDbPkg(c("hgu95av2.db", "hgu133a2.db"))

  ## A character vector of length 1 is treated as a regular expression.
  ## For example to make all the packages described in the master index:
  #makeAnnDbPkg(".*")
}

\keyword{methods}

