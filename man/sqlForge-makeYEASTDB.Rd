\name{makeYEASTCHIP_DB}

\alias{makeYEASTCHIP_DB}

\title{Creates a sqlite database, and then makes an annotation package
  with it}

\description{
  This function 1st creates a SQLite file useful for making a SQLite
  based annotation package by using the correct popXXXCHIP\_DB function.
  Next, this function produces an annotation package featuring the
  sqlite database produced.
}

\usage{
  makeYEASTCHIP_DB(prefix, fileName, chipSrc, outputDir, version,
  manufacturer, chipName, manufacturerUrl)
}

\arguments{
  \item{prefix}{
    prefix is the first part of the eventual desired package name.
  (ie. "prefix.db")
  }
  \item{fileName}{
    The path and filename for the file to be parsed.  This can either be
  an affy csv file or it can be a more classic file type.
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
makeHUMANCHIP_DB(affy = TRUE,
                 prefix = "hgu95av2",
                 fileName = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/HG_U95Av2_annot.csv.070824",
                 otherSrc = c(
                   EA="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/hgu95av2.EA.txt",
                   UMICH="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/hgu95av2_UMICH.txt"),
                 chipMapSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipmapsrc_human.sqlite",
                 chipSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipsrc_human.sqlite",
                 baseMapType = "gbNRef",
                 version = "1.0.0",
                 manufacturer = "Affymetrix",
                 chipName = "hgu95av2",
                 manufacturerUrl = "http://www.affymetrix.com")
}
}

\keyword{utilities}
