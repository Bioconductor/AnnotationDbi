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
\alias{columns}
\alias{columns,AnnotationDb-method}
\alias{columns,OrgDb-method}
\alias{columns,ChipDb-method}
\alias{columns,GODb-method}
\alias{columns,InparanoidDb-method}
\alias{columns,Inparanoid8Db-method}
\alias{columns,ReactomeDb-method}

\alias{keytypes}
\alias{keytypes,OrgDb-method}
\alias{keytypes,ChipDb-method}
\alias{keytypes,GODb-method}
\alias{keytypes,InparanoidDb-method}
\alias{keytypes,Inparanoid8Db-method}
\alias{keytypes,ReactomeDb-method}

% \alias{keys} %Not aliased here because it is already aliased in Bimap-keys.Rd
% \alias{keys,AnnotationDb-method}
\alias{keys}
\alias{keys,OrgDb-method}
\alias{keys,ChipDb-method}
\alias{keys,GODb-method}
\alias{keys,InparanoidDb-method}
\alias{keys,Inparanoid8Db-method}
\alias{keys,ReactomeDb-method}

\alias{select}
% \alias{select,AnnotationDb-method}
\alias{select,OrgDb-method}
\alias{select,ChipDb-method}
\alias{select,GODb-method}
\alias{select,InparanoidDb-method}
\alias{select,Inparanoid8Db-method}
\alias{select,ReactomeDb-method}

% mapIds should work for anything with select defined...
\alias{mapIds}
\alias{mapIds,AnnotationDb-method}




\title{AnnotationDb objects and their progeny, methods etc.}

\description{
  \code{AnnotationDb} is the virtual base class for all annotation
  packages.  It contain a database connection and is meant to be the
  parent for a set of classes in the Bioconductor annotation
  packages. These classes will provide a means of dispatch for a
  widely available set of \code{select} methods and thus allow the
  easy extraction of data from the annotation packages.

  \code{select}, \code{columns} and \code{keys} are used together to
  extract data from an \code{AnnotationDb} object (or any object derived
  from the parent class).  Examples of classes derived from the
  \code{AnnotationDb} object include (but are not limited to):
  \code{ChipDb}, \code{OrgDb} \code{GODb}, \code{InparanoidDb} and
  \code{ReactomeDb}.

  \code{columns} shows which kinds of data can be returned for the
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
  parameters for selected \code{keys} and \code{columns} and \code{keytype}
  arguments.

  \code{mapIds} gets the mapped ids (column) for a set of keys that are of a 
  particular keytype.  Usually returned as a named character vector.

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
  columns(x)
  keytypes(x)
  keys(x, keytype, ...)
  select(x, keys, columns, keytype, ...)
  mapIds(x, keys, column, keytype, ..., multiVals)
  saveDb(x, file)
  loadDb(file, dbType, dbPackage, ...)
}

\arguments{
  \item{x}{the \code{AnnotationDb} object. But in practice this will mean an 
    object derived from an \code{AnnotationDb} object such as a \code{OrgDb} or 
    \code{ChipDb} object.}
  \item{keys}{the keys to select records for from the database.  All possible 
    keys are returned by using the \code{keys} method.}
  \item{columns}{the columns or kinds of things that can be retrieved
    from the database.  As with \code{keys}, all possible columns are
    returned by using the \code{columns} method.}
  \item{keytype}{the keytype that matches the keys used.  For the
    \code{select} methods, this is used to indicate the kind of ID being used
    with the keys argument. For the \code{keys} method this is used to
    indicate which kind of keys are desired from \code{keys}
  }
  \item{column}{the column to search on (for \code{mapIds}).  Different from 
    \code{columns} in that it can only have a single element for the value}
  \item{file}{an \code{sqlite} file path.  A string the represents the
  full name you want for your sqlite database and also where to put it.}
  \item{dbType}{dbType - not required}
  \item{dbPackage}{dbPackage - not required}
  \item{...}{other arguments.  These include:
    \describe{
      \item{pattern:}{the pattern to match (used by keys)}
      \item{column:}{the column to search on. This is used by keys and is
	for when the thing you want to pattern match is different from
	the keytype, or when you want to simply want to get keys that
	have a value for the thing specified by the column argument.}
      \item{fuzzy:}{TRUE or FALSE value.  Use fuzzy matching? (this is
	used with pattern by the keys method)}
    }
  }
  \item{multiVals}{What should \code{mapIds} do when there are multiple values         
  that could be returned?  Options include:
    \describe{
      \item{first:}{This value means that when there are multiple matches only the 1st thing that comes back will be returned. This is the default behavior}
      \item{list:}{This will just returns a list object to the end user}
      \item{filter:}{This will remove all elements that contain multiple matches and will therefore return a shorter vector than what came in whenever some of the keys match more than one value}
      \item{asNA:}{This will return an NA value whenever there are multiple matches}
      \item{CharacterList:}{This just returns a SimpleCharacterList object}
      \item{FUN:}{You can also supply a function to the \code{multiVals} argument for custom behaviors.  The function must take a single argument and return a single value.  This function will be applied to all the elements and will serve a 'rule' that for which thing to keep when there is more than one element.  So for example this example function will always grab the last element in each result: \code{ last <- function(x){x[[length(x)]]} }
      }
    }
  }
}

\value{
  \code{keys},\code{columns} and \code{keytypes} each return a character
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
## display the columns
columns(hgu95av2.db)
## get the 1st 6 possible keys
keys <- head( keys(hgu95av2.db) )
keys
## lookup gene symbol and unigene ID for the 1st 6 keys
select(hgu95av2.db, keys=keys, columns = c("SYMBOL","UNIGENE"))

## get keys based on unigene
keyunis <- head( keys(hgu95av2.db, keytype="UNIGENE") )
keyunis
## list supported key types
keytypes(hgu95av2.db)
## lookup gene symbol and unigene ID based on unigene IDs by setting
## the keytype to "UNIGENE" and passing in unigene keys:
select(hgu95av2.db, keys=keyunis, columns = c("SYMBOL","UNIGENE"),
       keytype="UNIGENE")

keys <- head(keys(hgu95av2.db, 'ENTREZID'))
## get a default result (captures only the 1st element)
mapIds(hgu95av2.db, keys=keys, column='ALIAS', keytype='ENTREZID')
## or use a different option
mapIds(hgu95av2.db, keys=keys, column='ALIAS', keytype='ENTREZID', 
    multiVals="CharacterList")
## Or define your own function
last <- function(x){x[[length(x)]]}
mapIds(hgu95av2.db, keys=keys, column='ALIAS', keytype='ENTREZID', 
    multiVals=last)

}


\keyword{classes}
\keyword{methods}

