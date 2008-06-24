\name{popHUMANDB}

\alias{popHUMANDB}
\alias{popMALARIADB}
\alias{popMOUSEDB}
\alias{popRATDB}
\alias{popFLYDB}
\alias{popYEASTDB}
\alias{popZEBRAFISHDB}
\alias{popECOLIDB}

\title{Populates an SQLite DB with and produces a schema definition}

\description{
  Creates SQLite file useful for making a SQLite based annotation
  package.  Also produces the schema file which details the schema for
  the database produced.
}

\usage{
  popHUMANDB(prefix, chipSrc, metaDataSrc, outputDir, printSchema)
}

\arguments{
  \item{prefix}{
    prefix is the first part of the eventual desired package name.
  (ie. "prefix.sqlite")
  }
  \item{chipSrc}{
    The path and filename to the intermediate database containing the
  annotation data for the sqlite to build.
  }
  \item{metaDataSrc}{
    Either a named character vector containing pertinent information
  about the metadata OR the path and filename to the intermediate
  database containing the metadata information for the package.

  If this is a custom package, then it must be a named vector with the
  following fields:

  metaDataSrc <- c( DBSCHEMA="the DB schema",
                    ORGANISM="the organism",
                    SPECIES="the species",
                    MANUFACTURER="the manufacturer",
                    CHIPNAME="the chipName",
                    MANUFACTURERURL="the manufacturerUrl")

  }
  \item{outputDir}{
    Where you would like the output files to be placed.
  }
  \item{printSchema}{
    Boolean to indicate whether or not to produce an output of the
  schema (default is FALSE).
  }
}


\examples{
\dontrun{
  ##Set up the metadata
  my_metaDataSrc <- c( DBSCHEMA="the DB schema",
                    ORGANISM="the organism",
                    SPECIES="the species",
                    MANUFACTURER="the manufacturer",
                    CHIPNAME="the chipName",
                    MANUFACTURERURL="the manufacturerUrl")  

  ##Builds the org.Hs.eg sqlite:
  popHUMANDB(prefix="org.Hs.eg",
             chipSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipsrc_human.sqlite",
             metaDataSrc = my_metaDataSrc,
             printSchema=TRUE)

  ##Or if the package is a standard package (it probably isn't):
  ##Builds the org.Hs.eg sqlite:
  popHUMANDB(prefix="org.Hs.eg",
             chipSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipsrc_human.sqlite",
             metaDataSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/metadatasrc.sqlite",
             printSchema=TRUE)
}
}

\keyword{utilities}

