\name{makeARABIDOPSISCHIP_DB}

\alias{makeARABIDOPSISCHIP_DB}

\title{Creates a sqlite database, and then makes an annotation package
  with it}

\description{
  This function 1st creates a SQLite file useful for making a SQLite
  based annotation package by using the correct popXXXCHIP\_DB function.
  Next, this function produces an annotation package featuring the
  sqlite database produced.  This function REQUIRES that you have
  installed the latest arabidopsis.dbo package which can be obtained by
  using biocLite().
}

\usage{
  makeARABIDOPSISCHIP_DB(affy, prefix, fileName, chipMapSrc, chipSrc,
  outputDir, version, manufacturer, chipName, manufacturerUrl, author,
  maintainer)
}

\arguments{
  \item{affy}{
    Boolean to indicate if this is starting from an affy csv file or
  not.  If it is, then that will be parsed to make the sqlite file, if
  not, then you can feed a tab delimited file with IDs as was done
  before with AnnBuilder.
  }
  \item{prefix}{
    prefix is the first part of the eventual desired package name.
  (ie. "prefix.db")
  }
  \item{fileName}{
    The path and filename for the file to be parsed.  This field can be
  ommitted if affy is set to TRUE.  For all other arabidopsis chips, the
  IDs that match to the probes MUST be TAIR IDs.
  }
  \item{chipMapSrc}{
    The path and filename to the intermediate database containing the
  mapping data for allowed ID types and how these IDs relate to each other.
  }
  \item{chipSrc}{
    The path and filename to the intermediate database containing the
  annotation data for the sqlite to build.
  }
  \item{outputDir}{
    Where you would like the output files to be placed.
  }
  \item{version}{
    What is the version number for the desired package.
  }
  \item{manufacturer}{
    Who made the chip being described.
  }
  \item{chipName}{
    What is the name of the chip.
  }
  \item{manufacturerUrl}{
    URL for manufacturers website.
  }
  \item{author}{
    List of authors involved in making the package.
  }
  \item{maintainer}{
    List of package maintainers with email addresses for contact purposes.
  }
}


\examples{
\dontrun{

}
}

\keyword{utilities}
