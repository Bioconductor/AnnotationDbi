\name{popYEASTCHIPDB}

\alias{popYEASTCHIPDB}

\title{Populates an SQLite DB with and produces a schema definition}

\description{
  Creates SQLite file useful for making a SQLite based annotation
  package.  Also produces the schema file which details the schema for
  the database produced.
}

\usage{
  popYEASTCHIPDB(prefix, fileName, chipSrc, metaDataSrc, outputDir,
  printSchema)
}

\arguments{
  \item{prefix}{
    prefix is the first part of the eventual desired package name.
  (ie. "prefix.sqlite")
  }
  \item{fileName}{
    The path and filename for the file to be parsed.  This can either be
  an affy csv file or it can be a more classic file type.
  }
  \item{chipSrc}{
    The path and filename to the intermediate database containing the
  annotation data for the sqlite to build.
  }
  \item{metaDataSrc}{
    The path and filename to the intermediate database containing the
  metadata information for the package.
  }
  \item{outputDir}{
    Where you would like the output files to be placed.
  }
  \item{printSchema}{
    Boolean to indicate whether or not to produce an output of the
    schema (default is FALSE).
  }
}

\details{
}

\examples{
\dontrun{
  ##Builds the yeast2 sqlite:
  popYEASTCHIPDB(fileName="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/yeast2/Yeast_2_annot.csv.070824",
                 prefix="yeast2",
                 chipSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipsrc_yeast.sqlite",
                 metaDataSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/metadatasrc.sqlite",
                 printSchema=TRUE)
}
}

\keyword{utilities}
