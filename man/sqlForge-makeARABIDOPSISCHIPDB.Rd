\name{makeARABIDOPSISCHIP_DB}

\alias{makeARABIDOPSISCHIP_DB}

\title{Creates a sqlite database, and then makes an annotation package
  with it}

\description{
  This function 1st creates a SQLite file useful for making a SQLite
  based annotation package by using the correct popXXXCHIP\_DB function.
  Next, this function produces an annotation package featuring the
  sqlite database produced.
}

\usage{
  makeARABIDOPSISCHIP_DB(affy, prefix, fileName, chipMapSrc, chipSrc,
  outputDir, version, manufacturer, chipName, manufacturerUrl)
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
    The path and filename for the file to be parsed.  This can either be
  an affy csv file or it can be a more classic file type.
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
}


\examples{
\dontrun{

}
}

\keyword{utilities}
