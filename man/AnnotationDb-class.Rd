\name{AnnotationDb-objects}
\docType{class}

% Classes
\alias{AnnotationDb}
\alias{class:AnnotationDb}
\alias{AnnotationDb-class}

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

% select and select related methods
\alias{cols}
\alias{cols,AnnotationDb-method}
\alias{cols,OrgDb-method}
\alias{cols,ChipDb-method}
\alias{cols,GODb-method}
\alias{cols,InparanoidDb-method}

% \alias{keys} %Not aliased here because it is already aliased in Bimap-keys.Rd
% \alias{keys,AnnotationDb-method}
\alias{keys,OrgDb-method}
\alias{keys,ChipDb-method}
\alias{keys,GODb-method}
\alias{keys,InparanoidDb-method}

\alias{select}
% \alias{select,AnnotationDb-method}
\alias{select,OrgDb-method}
\alias{select,ChipDb-method}
\alias{select,GODb-method}
\alias{select,InparanoidDb-method}



\title{AnnotationDb objects and their progeny, methods etc.}

\description{
  \code{AnnotationDb} is the virtual base class for all annotation
  packages.  It contain a database connection and is meant to be the
  parent for a set of classes in the Bioconductor annotation
  packages. These classes will provide a means of dispatch for a
  widely available set of \code{select} methods and thus allow the
  easy extraction of data from the annotation packages.

  \code{select}, \code{cols} and \code{keys} are used together to
  extract data from an \code{AnnotationDb} object (or any object
  derived from the parent class).  Examples of classes derived from
  the \code{AnnotationDb} object include (but are not limited to):
  \code{ChipDb}, \code{OrgDb}, \code{GODb} and \code{InparanoidDb}

  \code{keys} returns the primary keys for the database contained in
  the \code{AnnotationDb} object.  This method is already documented
  in the keys manual page but is mentioned again here because it's
  usage with \code{select} is so intimate.
  
  \code{cols} shows which kinds of data can be returned for the
  \code{AnnotationDb} object.

  \code{select} will retrieve the data as a data.frame based on
  parameters for selected keys and cols.
}

\usage{
  cols(x)
  select(x, keys, cols, ...)  
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
  \item{...}{other arguments}
}

\value{
  \code{keys} and \code{cols} each return a character vector or possible values.  \code{select} returns a data.frame.
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
## lookup gene symbol and unigene ID for the 1st 6 keys.
select(hgu95av2.db, keys=keys, cols = c("SYMBOL","UNIGENE"))
}


\keyword{classes}
\keyword{methods}

