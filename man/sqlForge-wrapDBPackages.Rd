\name{wrapBaseDBPackages}
\alias{wrapBaseDBPackages}

\title{Wrap up all the Base Databases into Packages for distribution}

\description{
  Creates extremely simple packages from the base database files for
  distribution.  This is a convenience function for wrapping up these
  packages in a consistent way each time.
}

\usage{
  wrapBaseDBPackages(dbPath, destDir, version)
}

\arguments{
  \item{dbPath}{
    dbPath is just the path to the location of the latest intermediate
    sqlite source files. These files are then used to make base DB
    packages. 
  }
  \item{destDir}{
    destination path for the newly minted packages. 
  }  
  \item{version}{
    version number to stamp onto these newly minted packages. 
  }  
}

\examples{
\dontrun{
  ##Make all of the intermediate DBs and place the new packages right here.
  wrapBaseDBPackages(dbPath = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/",
                     destDir = ".")
}
}

\keyword{utilities}
