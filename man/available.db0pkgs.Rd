\name{available.db0pkgs}

\alias{available.db0pkgs}

\title{available.db0pkgs}

\description{
  Get the list of intermediate annotation data packages (.db0 data packages)
  that are currently available on the Bioconductor repositories for your
  version of R/Bioconductor.
}

\usage{
  available.db0pkgs()
}


\details{
  The SQLForge code uses a series of intermediate database packages that are
  necessary to build updated custom annotation packages.  These packages must
  be installed or updated if you want to make a custom annotation package for
  a particular organism.  These special intermediate packages contain the
  latest freeze of the data needed to build custom annotation data packages
  and are easily identified by the fact that they end with the special ".db0"
  suffix.  This function will list all such packages that are available for a
  specific version of bioconductor.
}

\value{
  A character vector containing the names of the available ".db0" data
  packages.  
}

\author{H. Pages and Marc Carlson}

\examples{
  # Get the list of BSgenome data packages currently available:
  available.db0pkgs()

  \dontrun{
    # Make your choice and install like this:
    source("http://bioconductor.org/biocLite.R")
    biocLite("human.db0")
  }
}

\keyword{manip}
