\name{createSimpleBimap}

\alias{createSimpleBimap}

\title{Creates a simple Bimap from a SQLite database in an situation that
  is external to AnnotationDbi}

\description{
  This function allows users to easily make a simple Bimap object for
  extra tables etc that they may wish to add to their annotation
  packages.  For most Bimaps, their definition is stored
  inside of AnnotationDbi.  The addition of this function is to help
  ensure that this does not become a limitation, by allowing simple
  extra Bimaps to easily be defined external to AnnotationDbi.  Usually,
  this will be done in the zzz.R source file of a package so that these
  extra mappings can be seemlessly integrated with the rest of the
  package.  For now, this function assumes that users will want to use
  data from just one table.  
}

\usage{
  createSimpleBimap(tablename, Lcolname, Rcolname, datacache, objName,
  objTarget)
}

\arguments{
  \item{tablename}{
    The name of the database table to grab the mapping information from. 
  }
  \item{Lcolname}{
    The field name from the database table.  These will become the Lkeys
  in the final mapping.
  }
  \item{Rcolname}{
    The field name from the database table.  These will become the Rkeys
  in the final mapping.
  }
  \item{datacache}{
    The datacache object should already exist for every standard
  Annotation package.  It is not exported though, so you will have to
  access it with ::: . It is needed to provide the connection
  information to the function.
  }
  \item{objName}{
    This is the name of the mapping.
  }
  \item{objTarget}{
    This is the name of the thing the mapping goes with. For most uses,
  this will mean the package name that the mapping belongs with.
  }
}

\examples{
##You simply have to call this function to create a new mapping.  For
##example, you could have created a mapping between the gene_name and
##the symbols fields from the gene_info table contained in the hgu95av2
##package by doing this:
library(hgu95av2.db)
hgu95av2NAMESYMBOL <- createSimpleBimap("gene_info",
                                        "gene_name",
                                        "symbol",
                                        hgu95av2.db:::datacache,
                                        "NAMESYMBOL",
                                        "hgu95av2.db")

}

\keyword{utilities}
