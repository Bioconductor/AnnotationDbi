\name{AnnDbPkg-maker}

\alias{AnnDbPkg-maker}

\alias{AnnDbPkgSeed}
\alias{class:AnnDbPkgSeed}
\alias{AnnDbPkgSeed-class}

\alias{makeAnnDbPkg}
\alias{makeAnnDbPkg,AnnDbPkgSeed-method}
\alias{makeAnnDbPkg,list-method}
\alias{makeAnnDbPkg,character-method}
\alias{loadAnnDbPkgIndex}


\title{Creates an SQLite-based annotation package}

\description{
  Creates an SQLite-based annotation package from an SQLite file.
}

\usage{
  makeAnnDbPkg(x, dbfile, dest_dir=".", no.man=FALSE, ...)
  loadAnnDbPkgIndex(file)
}

\arguments{
  \item{x}{
    A AnnDbPkgSeed object, a list, a string or a regular expression.
  }
  \item{dbfile}{
    The path to the SQLite containing the annotation data for the package
    to build.
  }
  \item{dest_dir}{
    The directory where the package will be created.
  }
  \item{file}{
    The path to a DCF file containing the list of annotation packages
    to build.
  }
  \item{no.man}{
    If \code{TRUE} then no man page is included in the package.
  }
  \item{...}{
    Extra args used for extra filtering.
  }
}


\seealso{
  \link{AnnDbPkg-checker}
}

\examples{
  ## With a "AnnDbPkgSeed" object:
  seed <- new("AnnDbPkgSeed",
      Package="hgu133a2.db",
      Version="0.0.99",
      PkgTemplate="HUMANCHIP.DB",
      AnnObjPrefix="hgu133a2"
  )
  if (FALSE)
      makeAnnDbPkg(seed, "path/to/hgu133a2.sqlite")

  ## With package names:
  ## (Note that in this case makeAnnDbPkg() will use the package descriptions
  ## found in the master index file ANNDBPKG-INDEX.TXT located in the
  ## AnnotationDbi package.)
  if (FALSE)
      makeAnnDbPkg(c("hgu95av2.db", "hgu133a2.db"))

  ## A character vector of length 1 is treated as a regular expression:
  if (FALSE)
      makeAnnDbPkg("hgu.*")
  ## To make all the packages described in the master index:
  if (FALSE)
      makeAnnDbPkg("")
  ## Extra args can be used to narrow down the roaster of packages to make:
  if (FALSE) {
      makeAnnDbPkg("", PkgTemplate="HUMANCHIP.DB", manufacturer="Affymetrix")
      makeAnnDbPkg(".*[3k]\\\\.db", species=c("Mouse", "Rat"))
  }

  ## The master index file ANNDBPKG-INDEX.TXT can be loaded with:
  loadAnnDbPkgIndex()
}

\keyword{utilities}
\keyword{classes}
\keyword{methods}

