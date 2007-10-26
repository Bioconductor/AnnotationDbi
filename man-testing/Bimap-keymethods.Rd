\name{Bimap-keymethods}

\alias{Bimap-keymethods}

\alias{keys}
\alias{keys,Bimap-method}

\alias{length,Bimap-method}

\alias{mappedkeys}
\alias{mappedkeys,Bimap-method}
\alias{mappedkeys,environment-method}
\alias{mappedkeys,vector-method}

\alias{count.mappedkeys}
\alias{count.mappedkeys,Bimap-method}
\alias{count.mappedkeys,ANY-method}

\alias{isNA}
\alias{isNA,Bimap-method}
\alias{isNA,environment-method}
\alias{isNA,ANY-method}

\alias{Lkeys}
\alias{Lkeys,FlatBimap-method}
\alias{Lkeys,AnnDbBimap-method}

\alias{Rkeys}
\alias{Rkeys,FlatBimap-method}
\alias{Rkeys,AnnDbBimap-method}
\alias{Rkeys,Go3AnnDbBimap-method}
\alias{Rkeys,AnnDbMap-method}

\alias{Llength}
\alias{Llength,Bimap-method}
\alias{Llength,AnnDbBimap-method}

\alias{Rlength}
\alias{Rlength,Bimap-method}
\alias{Rlength,AnnDbBimap-method}
\alias{Rlength,Go3AnnDbBimap-method}
\alias{Rlength,AnnDbMap-method}

\alias{mappedLkeys}
\alias{mappedLkeys,FlatBimap-method}
\alias{mappedLkeys,AnnDbBimap-method}
\alias{mappedLkeys,Go3AnnDbBimap-method}
\alias{mappedLkeys,AgiAnnDbMap-method}

\alias{mappedRkeys}
\alias{mappedRkeys,FlatBimap-method}
\alias{mappedRkeys,AnnDbBimap-method}
\alias{mappedRkeys,Go3AnnDbBimap-method}
\alias{mappedRkeys,AnnDbMap-method}

\alias{count.mappedLkeys}
\alias{count.mappedLkeys,Bimap-method}
\alias{count.mappedLkeys,AnnDbBimap-method}
\alias{count.mappedLkeys,Go3AnnDbBimap-method}
\alias{count.mappedLkeys,AgiAnnDbMap-method}

\alias{count.mappedRkeys}
\alias{count.mappedRkeys,Bimap-method}
\alias{count.mappedRkeys,AnnDbBimap-method}
\alias{count.mappedRkeys,Go3AnnDbBimap-method}
\alias{count.mappedRkeys,AnnDbMap-method}

\alias{keys<-}
\alias{keys<-,Bimap-method}

\alias{[,Bimap-method}

\alias{Lkeys<-}
\alias{Lkeys<-,FlatBimap-method}
\alias{Lkeys<-,AnnDbBimap-method}

\alias{Rkeys<-}
\alias{Rkeys<-,FlatBimap-method}
\alias{Rkeys<-,AnnDbBimap-method}

\alias{subset}
\alias{subset,Bimap-method}
\alias{subset,AnnDbBimap-method}

\alias{show,AnnDbTable-method}


\title{Methods for manipulating the keys of a Bimap object}

\description{
  These methods are part of the \link{Bimap} interface.

  They are devided in 3 groups: directed \link{Bimap} methods,
  undirected \link{Bimap} methods and subsetting \link{Bimap} methods.
  Unlike for undirected methods, the result of a directed method
  depends on the \link{direction} of the \link{Bimap} object (left-to-right
  or right-to-left, see \code{\link{direction}} for more details).
  The subsetting methods allow to extract a submap from a \link{Bimap}
  object by restricting the mapping to a subset of keys.
}

\usage{
  ## Directed Bimap methods
  keys(x)
  #length(x)
  isNA(x)
  mappedkeys(x)
  count.mappedkeys(x)

  ## Undirected Bimap methods
  Lkeys(x)
  Rkeys(x)
  Llength(x)
  Rlength(x)
  mappedLkeys(x)
  mappedRkeys(x)
  count.mappedLkeys(x)
  count.mappedRkeys(x)

  ## Subsetting Bimap methods
  keys(x) <- value
  #x[i]
  Lkeys(x) <- value
  Rkeys(x) <- value
  subset(x, ...)
}

\arguments{
  \item{x}{
    A \link{Bimap} object.
  }
  \item{value}{
    A character vector containing the new keys (must be a subset of the
    current keys).
  }
  \item{i}{
    Index specifying map elements to extract.
  }
  \item{...}{
    Extra arguments can be:
    \describe{
      \item{\code{Lkeys}}{
        The new Lkeys (must be a subset of the current Lkeys).
      }
      \item{\code{Rkeys}}{
        The new Rkeys (must be a subset of the current Rkeys).
      }
      \item{\code{objName}}{
        The name to give to the submap (only supported if \code{x} is an
        \link{AnnDbBimap} object).
      }
    }
  }
}

\value{
}

\author{H. Pages}

\seealso{
  \link{Bimap},
  \link{BimapFormatting},
  \link{AnnDbBimap-envirAPI}
}

\examples{
  library(hgu95av2.db)
  x <- hgu95av2GO
  x
  length(x)
  count.mappedkeys(x)
  x[1:3]
  links(x[1:3])

  ## Keep only mapped keys
  keys(x) <- mappedkeys(x)
  length(x)
  count.mappedkeys(x)
  x # now it is a submap

  ## The above can be achieved with
  x <- hgu95av2GO[mappedkeys(hgu95av2GO)]

  ## mappedkeys() and count.mappedkeys() also work with an environment
  ## or a list
  z <- list(k1=NA, k2=letters[1:4], k3="x")
  mappedkeys(z)
  count.mappedkeys(z)
}

\keyword{methods}

