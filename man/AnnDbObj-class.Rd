\name{AnnDbObj-objects}
\docType{class}

\alias{AnnDbObj-objects}

% Classes
\alias{AnnDbObj}
\alias{class:AnnDbObj}
\alias{AnnDbObj-class}

% Accessor-like methods

\alias{dbconn}
\alias{dbconn,environment-method}
\alias{dbconn,AnnDbObj-method}

\alias{dbfile}
\alias{dbfile,environment-method}
\alias{dbfile,AnnDbObj-method}

\alias{dbmeta}
\alias{dbmeta,DBIConnection-method}
\alias{dbmeta,environment-method}
\alias{dbmeta,AnnDbObj-method}

\alias{dbschema}
\alias{dbschema,DBIConnection-method}
\alias{dbschema,environment-method}
\alias{dbschema,AnnDbObj-method}

\alias{dbInfo}
\alias{dbInfo,DBIConnection-method}
\alias{dbInfo,environment-method}
\alias{dbInfo,AnnDbObj-method}


\title{AnnDbObj objects}

\description{
  The AnnDbObj class is the most general container for storing any kind
  of SQLite-based annotation data.
}

\details{
  Many classes in AnnotationDbi inherit directly or indirectly
  from the AnnDbObj class. One important particular case is the
  \link{AnnDbBimap} class which is the lowest class in the AnnDbObj
  hierarchy to also inherit the \link{Bimap} interface.
}

\section{Accessor-like methods}{
  In the code snippets below,
  \code{x} is an AnnDbObj object.

  \describe{
    \item{}{
      \code{dbconn(x)}:
      Return a connection object to the SQLite DB containing \code{x}'s data.
    }
  }
  \describe{
    \item{}{
      \code{dbfile(x)}:
      Return the path (character string) to the SQLite DB (file) containing
      \code{x}'s data.
    }
  }
  \describe{
    \item{}{
      \code{dbmeta(x, name)}:
      Print the value of metadata whose name is 'name'.
      Also works if \code{x} is a DBIConnection object.
    }
  }
  \describe{
    \item{}{
      \code{dbschema(x, file="", show.indices=FALSE)}:
      Print the schema definition of the SQLite DB.
      Also works if \code{x} is a DBIConnection object.

      The \code{file} argument must be a connection, or a character string
      naming the file to print to (see the \code{file} argument of the
      \code{\link[base]{cat}} function for the details).

      The CREATE INDEX statements are not shown by default.
      Use \code{show.indices=TRUE} to get them.
    }
  }
  \describe{
    \item{}{
      \code{dbInfo(x)}:
      Prints other information about the SQLite DB.
      Also works if \code{x} is a DBIConnection object.
    }
  }
}

\seealso{
  \code{\link[DBI]{dbConnect}},
  \code{\link[DBI]{dbListTables}},
  \code{\link[DBI]{dbListFields}},
  \code{\link[DBI]{dbGetQuery}},
  \link{Bimap}
}

\examples{
  library(hgu95av2.db)

  dbconn(hgu95av2ENTREZID)              # same as hgu95av2_dbconn()
  dbfile(hgu95av2ENTREZID)              # same as hgu95av2_dbfile()

  dbmeta(hgu95av2_dbconn(), "ORGANISM")
  dbmeta(hgu95av2_dbconn(), "DBSCHEMA")
  dbmeta(hgu95av2_dbconn(), "DBSCHEMAVERSION")

  dbListTables(hgu95av2_dbconn())
  dbListFields(hgu95av2_dbconn(), "probes")
  dbListFields(hgu95av2_dbconn(), "genes")
  dbschema(hgu95av2ENTREZID)            # same as hgu95av2_dbschema()
  ## According to the schema, the probes.id column references the genes.id
  ## column. Note that in all tables, the "id" column is an internal id with
  ## no biological meaning (provided for allowing efficient joins between
  ## tables).
  ## To retrieve the mapping between manufacturer IDs and Entrez Gene IDs:
  dbGetQuery(hgu95av2_dbconn(), "SELECT * FROM probes INNER JOIN genes USING (id) LIMIT 10")
  ## This mapping is in fact the ENTREZID map:
  toTable(hgu95av2ENTREZID)[1:10, ] # only relevant columns are retrieved

  dbInfo(hgu95av2GO)                    # same as hgu95av2_dbInfo()
}

\keyword{classes}
\keyword{methods}

