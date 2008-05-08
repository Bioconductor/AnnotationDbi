\name{makeHUMANCHIP_DB}

\alias{makeHUMANCHIP_DB}
\alias{makeMOUSECHIP_DB}
\alias{makeRATCHIP_DB}
\alias{makeFLYCHIP_DB}

\title{Creates a sqlite database, and then makes an annotation package
  with it}

\description{
  This function 1st creates a SQLite file useful for making a SQLite
  based annotation package by using the correct popXXXCHIP\_DB function.
  Next, this function produces an annotation package featuring the
  sqlite database produced.
}

\usage{
  makeHUMANCHIP_DB(affy, prefix, fileName, otherSrc, chipMapSrc, chipSrc,
  baseMapType, outputDir, version, manufacturer, chipName,
  manufacturerUrl, author, maintainer)
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
  \item{otherSrc}{
    The path and filenames to any other lists of IDs which might add
  information about how a probe will map.  
  }
  \item{chipMapSrc}{
    The path and filename to the intermediate database containing the
  mapping data for allowed ID types and how these IDs relate to each other.    
  }
  \item{chipSrc}{
    The path and filename to the intermediate database containing the
  annotation data for the sqlite to build.
  }
  \item{baseMapType}{
    The type of ID that is used for the initial base mapping.  If using
  a classic base mapping file, this should be the ID type present in the
  fileName.  This can be any of the following values:
  "gb" = for genbank IDs
  "ug" = unigene IDs
  "eg" = Entrez Gene IDs
  "refseq" = refseq IDs
  "gbNRef" = mixture of genbank and refseq IDs
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
