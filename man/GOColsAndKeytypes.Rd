\name{GOID}

% \alias{GOID}
\alias{TERM}
\alias{ONTOLOGY}
\alias{DEFINITION}

\title{Descriptions of available values for \code{cols} and
  \code{keytypes} for GO.db.}


\description{
  This manual page enumerates the kinds of data represented by the
  values returned when the user calls \code{cols} or \code{keytypes}
}


\details{  
  All the possible values for \code{cols} and \code{keytypes} are listed
  below.  
  
  \describe{
    \item{GOID:}{GO Identifiers}
    \item{DEFINITION:}{The definition of a GO Term}
    \item{ONTOLOGY:}{Which of the three Gene Ontologies (BP, CC, or MF)}
    \item{TERM:}{The actual GO term}    
  }

  To get the latest  information about the date stamps and source URLS
  for the data used to make an annotation package, please use the
  metadata method as shown in the example below.
  
}


\author{Marc Carlson}


\examples{
  library(GO.db)
  ## List the possible values for cols
  cols(GO.db)
  ## List the possible values for keytypes
  keytypes(GO.db)
  ## get some values back
  keys <- head(keys(GO.db))
  keys
  select(GO.db, keys=keys, cols=c("TERM","ONTOLOGY"),
  keytype="GOID")

  ## More infomation about the dates and original sources for these data:
  metadata(GO.db)
}


\keyword{utilities}
\keyword{manip}
