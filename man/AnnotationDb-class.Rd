\name{AnnotationDb-objects}
\docType{class}

% Classes
\alias{AnnotationDb}
\alias{class:AnnotationDb}
\alias{AnnotationDb-class}
\alias{ChipDb-class}
\alias{GODb-class}
\alias{InparanoidDb-class}
\alias{OrgDb-class}
\alias{ReactomeDb-class}

% Accessor-like methods
\alias{loadDb}
\alias{loadDb,character,character,character-method}
\alias{loadDb,character,missing,missing-method}
\alias{dbConn,AnnotationDb-method}
\alias{saveDb}
\alias{saveDb,AnnotationDb-method}
\alias{show,AnnotationDb-method}
\alias{metadata,AnnotationDb-method}
\alias{names,AnnotationDb-method}
\alias{species}
\alias{species,AnnotationDb-method}


% select and select related methods
\alias{cols}
\alias{cols,AnnotationDb-method}
\alias{cols,OrgDb-method}
\alias{cols,ChipDb-method}
\alias{cols,GODb-method}
\alias{cols,InparanoidDb-method}
\alias{cols,ReactomeDb-method}

\alias{keytypes}
\alias{keytypes,OrgDb-method}
\alias{keytypes,ChipDb-method}
\alias{keytypes,GODb-method}
\alias{keytypes,InparanoidDb-method}
\alias{keytypes,ReactomeDb-method}

% \alias{keys} %Not aliased here because it is already aliased in Bimap-keys.Rd
% \alias{keys,AnnotationDb-method}
\alias{keys,OrgDb-method}
\alias{keys,ChipDb-method}
\alias{keys,GODb-method}
\alias{keys,InparanoidDb-method}
\alias{keys,ReactomeDb-method}

\alias{select}
% \alias{select,AnnotationDb-method}
\alias{select,OrgDb-method}
\alias{select,ChipDb-method}
\alias{select,GODb-method}
\alias{select,InparanoidDb-method}
\alias{select,ReactomeDb-method}



\title{AnnotationDb objects and their progeny, methods etc.}

\description{
  \code{AnnotationDb} is the virtual base class for all annotation
  packages.  It contain a database connection and is meant to be the
  parent for a set of classes in the Bioconductor annotation
  packages. These classes will provide a means of dispatch for a
  widely available set of \code{select} methods and thus allow the
  easy extraction of data from the annotation packages.

  \code{select}, \code{cols} and \code{keys} are used together to
  extract data from an \code{AnnotationDb} object (or any object derived
  from the parent class).  Examples of classes derived from the
  \code{AnnotationDb} object include (but are not limited to):
  \code{ChipDb}, \code{OrgDb} \code{GODb}, \code{InparanoidDb} and
  \code{ReactomeDb}.

  \code{cols} shows which kinds of data can be returned for the
  \code{AnnotationDb} object.
  
  \code{keytypes} allows the user to discover which keytypes can be
  passed in to \code{select} or \code{keys} and the \code{keytype}
  argument.
  
  \code{keys} returns keys for the database contained in the
  \code{AnnotationDb} object .  This method is already documented in the
  keys manual page but is mentioned again here because it's usage with
  \code{select} is so intimate.  By default it will return the primary
  keys for the database, but if used with the \code{keytype} argument,
  it will return the keys from that keytype.
  
  \code{select} will retrieve the data as a data.frame based on
  parameters for selected \code{keys} and \code{cols} and \code{keytype}
  arguments.

  \code{saveDb} will take an AnnotationDb object and save the database
  to the file specified by the path passed in to the \code{file}
  argument.
  
  \code{loadDb} takes a .sqlite database file as an argument and uses
  data in the metadata table of that file to return an AnnotationDb
  style object of the appropriate type.

  \code{species} shows the genus and species label currently attached to
  the \code{AnnotationDb} objects database.
  
}

\usage{
  cols(x)
  keytypes(x)
  select(x, keys, cols, keytype, ...)

  saveDb(x, file)
  loadDb(file, dbType, dbPackage, ...)
}

\arguments{
  \item{x}{the \code{AnnotationDb} object. But in practice this will mean an 
    object derived from an \code{AnnotationDb} object such as a \code{OrgDb} or 
    \code{ChipDb} object.}
  \item{keys}{the keys to select records for from the database.  All possible 
    keys are returned by using the \code{keys} method.}
  \item{cols}{the cols or kinds of things that can be retrieved from the 
    database.  As with \code{keys}, all possible cols are returned by using the
    \code{cols} method.}
  \item{keytype}{the keytype that matches the keys used.  For the
    \code{select} methods, this is used to indicate the kind of ID being used
    with the keys argument. For the \code{keys} method this is used to
    indicate which kind of keys are desired from \code{keys}
  }
  \item{file}{an \code{sqlite} file path.  A string the represents the
  full name you want for your sqlite database and also where to put it.}
  \item{dbType}{dbType - not required}
  \item{dbPackage}{dbPackage - not required}
  \item{...}{other arguments}
  \item{pattern}{the pattern to match (used by keys)}
  \item{column}{the column to search on. This is used by keys and is for when
  the thing you want to pattern match is different from the keytype, or
  when you want to simply want to get keys that have a value for the
  thing specified by the column argument.}
  \item{fuzzy}{TRUE or FALSE value.  Use fuzzy matching? (this is used
  with pattern by the keys method)}
}

\value{
  \code{keys},\code{cols} and \code{keytypes} each return a character
    vector or possible values.  \code{select} returns a data.frame.
}

\author{Marc Carlson}

\seealso{
  \code{keys},
  \code{\link[DBI:dbConnect]{dbConnect}},
  \code{\link[DBI:dbListTables]{dbListTables}},
  \code{\link[DBI:dbListTables]{dbListFields}},
  \code{\link[DBI:dbSendQuery]{dbGetQuery}},
  \link{Bimap}
}

\examples{
require(hgu95av2.db)
## display the cols
cols(hgu95av2.db)
## get the 1st 6 possible keys
keys <- head( keys(hgu95av2.db) )
keys
## lookup gene symbol and unigene ID for the 1st 6 keys
select(hgu95av2.db, keys=keys, cols = c("SYMBOL","UNIGENE"))

## get keys based on unigene
keyunis <- head( keys(hgu95av2.db, keytype="UNIGENE") )
keyunis
## list supported key types
keytypes(hgu95av2.db)
## lookup gene symbol and unigene ID based on unigene IDs by setting
## the keytype to "UNIGENE" and passing in unigene keys:
select(hgu95av2.db, keys=keyunis, cols = c("SYMBOL","UNIGENE"),
       keytype="UNIGENE")

}


\keyword{classes}
\keyword{methods}

