\name{Bimap-toTable}

\alias{Bimap-toTable}

\alias{toTable}
\alias{toTable,FlatBimap-method}
\alias{toTable,AnnDbBimap-method}

\alias{head}
\alias{head,FlatBimap-method}

\alias{tail}
\alias{tail,FlatBimap-method}

\alias{links}
\alias{links,FlatBimap-method}
\alias{links,AnnDbBimap-method}
\alias{links,Go3AnnDbBimap-method}

\alias{nrow}
\alias{nrow,FlatBimap-method}
\alias{nrow,AnnDbTable-method}
\alias{nrow,AnnDbBimap-method}
\alias{nrow,Go3AnnDbBimap-method}

\alias{dim,Bimap-method}

\alias{count.links}
\alias{count.links,Bimap-method}
\alias{count.links,Go3AnnDbBimap-method}

\alias{as.character,AtomicAnnDbBimap-method}


\title{Methods for extracting data from a Bimap object}

\description{
  These methods are part of the \link{Bimap} API.

  They can be used to extract data from a \link{Bimap} object.
}

\usage{
  ## Extract the data (undirected methods)
  toTable(x)
  head(x, ...)
  tail(x, ...)
  links(x)

  ## Get the size of the data without extracting them (undirected methods)
  nrow(x)
  #dim(x)
  count.links(x)  

  ## Extract the data (directed method)
  #as.character(x, ...)
}

\arguments{
  \item{x}{
    A \link{Bimap} object.
  }
  \item{...}{
    Further arguments to be passed to or from other methods (see
    \code{\link[utils]{head}}, \code{\link[utils]{tail}} or
    \code{\link[base]{as.character}}
    for the details).
  }
}

\value{
}

\author{H. Pages}

\seealso{
  \link{Bimap-direction},
  \link{Bimap-colmethods},
  \link{Bimap-keymethods},
  \link{AnnDbBimap-envirAPI}
}

\examples{
  library(GO.db)
  x <- GOSYNONYM
  x
  toTable(x)[1:4, ]
  toTable(x["GO:0007322"])
  links(x)[1:4, ]
  links(x["GO:0007322"])

  nrow(x)
  dim(x)
  count.links(x)

  library(hgu95av2.db)
  as.character(hgu95av2ENTREZID)[1:9]
}

\keyword{methods}
