\name{subset}

\alias{keys}
\alias{keys,Bimap-method}
\alias{length}
\alias{length,Bimap-method}
\alias{mappedKeys}
\alias{mappedKeys,Bimap-method}
\alias{count.mappedKeys}
\alias{count.mappedKeys,Bimap-method}

\alias{left.keys}
\alias{left.keys,FlatBimap-method}
\alias{left.keys,AnnDbBimap-method}

\alias{right.keys}
\alias{right.keys,FlatBimap-method}
\alias{right.keys,AnnDbBimap-method}
\alias{right.keys,Go3AnnDbBimap-method}
\alias{right.keys,AnnDbMap-method}

\alias{left.length}
\alias{left.length,Bimap-method}
\alias{left.length,AnnDbBimap-method}

\alias{right.length}
\alias{right.length,Bimap-method}
\alias{right.length,AnnDbBimap-method}
\alias{right.length,Go3AnnDbBimap-method}
\alias{right.length,AnnDbMap-method}

\alias{left.mappedKeys}
\alias{left.mappedKeys,FlatBimap-method}
\alias{left.mappedKeys,AnnDbBimap-method}
\alias{left.mappedKeys,Go3AnnDbBimap-method}

\alias{right.mappedKeys}
\alias{right.mappedKeys,FlatBimap-method}
\alias{right.mappedKeys,AnnDbBimap-method}
\alias{right.mappedKeys,Go3AnnDbBimap-method}
\alias{right.mappedKeys,AnnDbMap-method}

\alias{count.left.mappedKeys}
\alias{count.left.mappedKeys,Bimap-method}
\alias{count.left.mappedKeys,AnnDbBimap-method}
\alias{count.left.mappedKeys,Go3AnnDbBimap-method}

\alias{count.right.mappedKeys}
\alias{count.right.mappedKeys,Bimap-method}
\alias{count.right.mappedKeys,AnnDbBimap-method}
\alias{count.right.mappedKeys,Go3AnnDbBimap-method}
\alias{count.right.mappedKeys,AnnDbMap-method}

\alias{keys<-}
\alias{keys<-,Bimap-method}

\alias{left.keys<-}
\alias{left.keys<-,FlatBimap-method}
\alias{left.keys<-,AnnDbBimap-method}

\alias{right.keys<-}
\alias{right.keys<-,FlatBimap-method}
\alias{right.keys<-,AnnDbBimap-method}

\alias{subset}
\alias{subset,FlatBimap-method}
\alias{subset,AnnDbBimap-method}


\title{Methods for manipulating the keys of a Bimap object}

\description{
  These methods are devided in 3 groups: directed, undirected and subsetting
  methods. Unlike for undirected methods, the result of a directed method
  depends on the direction of the Bimap object (left-to-right or
  right-to-left, see \code{\link{direction}} for more details).
  The subsetting methods allow to extract a submap from a Bimap object
  by restricting the mapping to a subset of keys.
}

\usage{
  ## Directed methods
  keys(x)
  length(x)
  mappedKeys(x)
  count.mappedKeys(x)

  ## Undirected methods
  left.keys(x)
  right.keys(x)
  left.length(x)
  right.length(x)
  left.mappedKeys(x)
  right.mappedKeys(x)
  count.left.mappedKeys(x)
  count.right.mappedKeys(x)

  ## Subsetting methods
  keys(x) <- value
  left.keys(x) <- value
  right.keys(x) <- value
  subset(x, left.keys=NULL, right.keys=NULL, objName=NULL)
}

\value{
}

\author{H. Pages}

\seealso{
  \code{\link{direction}}
}

\examples{
  library(hgu95av2.db)
  x <- hgu95av2GO
  x
  length(x)
  count.mappedKeys(x)

  ## Keep only mapped keys
  keys(x) <- mappedKeys(x)
  length(x)
  count.mappedKeys(x)
  x                     # note the difference?
}

\keyword{methods}

