\name{popHUMANCHIPDB}

\alias{popHUMANCHIPDB}
\alias{popMOUSECHIPDB}
\alias{popRATCHIPDB}
\alias{popFLYCHIPDB}

\title{Populates an SQLite DB with and produces a schema definition}

\description{
  Creates SQLite file useful for making a SQLite based annotation
  package.  Also produces the schema file which details the schema for
  the database produced.
}

\usage{
  popHUMANCHIPDB(affy, prefix, fileName, chipMapSrc, chipSrc, metaDataSrc,
  otherSrc, baseMapType, outputDir, printSchema)
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
  (ie. "prefix.sqlite")
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
  \item{otherSrc}{
    The path and filenames to any other lists of IDs which might add
  information about how a probe will map.
  }
  \item{baseMapType}{
    The type of ID that is used in the initial base mapping.  This would
  be the ID type present in the fileName.
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
  popHUMANCHIPDB(affy=TRUE,
                 prefix="hgu95av2",
                 fileName="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/HG_U95Av2_annot.csv.070824",
                 chipMapSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipmapsrc_human.sqlite",
                 chipSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipsrc_human.sqlite",
                 metaDataSrc = my_metaDataSrc,
                 otherSrc=c(
                   EA="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/hgu95av2.EA.txt",
                   UMICH="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/hgu95av2_UMICH.txt"),
                 printSchema=TRUE)

  ##Or if the package is a standard package (it probably isn't):
  popHUMANCHIPDB(affy=TRUE,
                 prefix="hgu95av2",
                 fileName="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/HG_U95Av2_annot.csv.070824",
                 chipMapSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipmapsrc_human.sqlite",
                 chipSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/chipsrc_human.sqlite",
                 metaDataSrc = "/mnt/cpb_anno/mcarlson/proj/sqliteGen/nli/annosrc/db/metadatasrc.sqlite",
                 otherSrc=c(
                   EA="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/hgu95av2.EA.txt",
                   UMICH="/mnt/cpb_anno/mcarlson/proj/sqliteGen/srcFiles/hgu95av2/hgu95av2_UMICH.txt"),
                 printSchema=TRUE)
}
}

\keyword{utilities}
