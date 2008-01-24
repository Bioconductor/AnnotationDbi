\name{popHUMANDB}

\alias{popHUMANDB}
\alias{popMOUSEDB}
\alias{popRATDB}
\alias{popFLYDB}
\alias{popYEASTDB}

\title{Populates an SQLite DB with and produces a schema definition}

\description{
  Creates SQLite file useful for making a SQLite based annotation
  package.  Also produces the schema file which details the schema for
  the database produced.
}

\usage{
  popHUMANDB(prefix, chipSrc, metaDataSrc, printSchema)
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
    The path and filename to the intermediate database containing the
  metadata information for the package.
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
  ##Builds the org.Hs.eg sqlite:
  popHUMANDB(prefix="org.Hs.eg",
             chipSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipsrc_human.sqlite",
             metaDataSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/metadatasrc.sqlite",
             printSchema=TRUE)
}
}

\keyword{utilities}

