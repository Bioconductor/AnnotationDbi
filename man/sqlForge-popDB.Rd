\name{populateDB}

\alias{populateDB}
\alias{popHUMANDB}
\alias{popMALARIADB}
\alias{popMOUSEDB}
\alias{popRATDB}
\alias{popFLYDB}
\alias{popYEASTDB}
\alias{popYEASTNCBIDB}
\alias{popZEBRAFISHDB}
\alias{popECOLIDB}
\alias{popCANINEDB}
\alias{popBOVINEDB}
\alias{popWORMDB}
\alias{popPIGDB}
\alias{popCHICKENDB}

\alias{popHUMANCHIPDB}
\alias{popMOUSECHIPDB}
\alias{popRATCHIPDB}
\alias{popFLYCHIPDB}
\alias{popZEBRAFISHCHIPDB}
\alias{popECOLICHIPDB}
\alias{popCANINECHIPDB}
\alias{popBOVINECHIPDB}
\alias{popWORMCHIPDB}
\alias{popPIGCHIPDB}
\alias{popCHICKENCHIPDB}



\title{Populates an SQLite DB with and produces a schema definition}

\description{
  Creates SQLite file useful for making a SQLite based annotation
  package.  Also produces the schema file which details the schema for
  the database produced.
}

\usage{
   populateDB(schema, ...)

#  usage case with required arguments
#  populateDB(schema, prefix, chipSrc, metaDataSrc)

#  usage case with all possible arguments
#  populateDB(schema, affy, prefix, fileName, chipMapSrc, chipSrc,
#  metaDataSrc, otherSrc, baseMapType, outputDir, printSchema)
}

\arguments{
  \item{schema}{
    String listing the schema that you want to use to make the DB.  You
  can list schemas with available.dbschemas()
  }
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
    The path and filename for the mapping file to be parsed.  This can
  either be an affy csv file or it can be a more classic file type.
  This is only needed when making chip packages.
  }
  \item{chipMapSrc}{
    The path and filename to the intermediate database containing the
  mapping data for allowed ID types and how these IDs relate to each
  other.  If not provided, then the appropriate source DB from the most
  current .db0 package will be used instead.
  }
  \item{chipSrc}{
    The path and filename to the intermediate database containing the
  annotation data for the sqlite to build.  If not provided, then the
  appropriate source DB from the most current .db0 package will be used
  instead.
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
  \item{printSchema}{
    Boolean to indicate whether or not to produce an output of the
    schema (default is FALSE).
  }
  \item{...}{
    Just used so we can have a wrapper function.  Ignore this argument.
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
  populateDB("HUMAN_DB",
             prefix="org.Hs.eg",
             chipSrc = "/mnt/cpb_anno/mcarlson/proj/mcarlson/sqliteGen/annosrc/db/chipsrc_human.sqlite",
             metaDataSrc = my_metaDataSrc,
             printSchema=TRUE)


  ##Builds hgu95av2.sqlite:
  populateDB("HUMANCHIP_DB",
             affy=TRUE,
             prefix="hgu95av2",
             fileName="/mnt/cpb_anno/mcarlson/proj/mcarlson/sqliteGen/srcFiles/hgu95av2/HG_U95Av2.na27.annot.csv",
             metaDataSrc=my_metaDataSrc,
             baseMapType="gbNRef")


  ##Builds the ag.sqlite:
  populateDB("ARABIDOPSISCHIP_DB",
             affy=TRUE,
             prefix="ag",
             metaDataSrc=my_metaDataSrc)


  ##Builds yeast2.sqlite:  
  populateDB("YEASTCHIP_DB",
              affy=TRUE,
              prefix="yeast2",
              fileName="/mnt/cpb_anno/mcarlson/proj/mcarlson/sqliteGen/srcFiles/yeast2/Yeast_2.na27.annot.csv",
              metaDataSrc=metaDataSrc)

}
}

\keyword{utilities}

