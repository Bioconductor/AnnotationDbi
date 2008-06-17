\name{Bimap-direction}

\alias{Bimap-direction}

\alias{direction}
\alias{direction,FlatBimap-method}
\alias{direction,AnnDbBimap-method}

\alias{direction<-}
\alias{direction<-,FlatBimap-method}
\alias{direction<-,AnnDbBimap-method}
\alias{direction<-,AnnDbMap-method}

\alias{revmap}
\alias{revmap,Bimap-method}
\alias{revmap,AnnDbBimap-method}
\alias{revmap,environment-method}
\alias{revmap,list-method}

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

\alias{Lkeys<-}
\alias{Lkeys<-,FlatBimap-method}
\alias{Lkeys<-,AnnDbBimap-method}

\alias{Rkeys<-}
\alias{Rkeys<-,FlatBimap-method}
\alias{Rkeys<-,AnnDbBimap-method}

\alias{subset}
\alias{subset,Bimap-method}
\alias{subset,AnnDbBimap-method}


\title{Methods for getting/setting the direction of a Bimap object,
       and undirected methods for getting/counting/setting its keys}

\description{
  These methods are part of the \link{Bimap} interface
  (see \code{?\link{Bimap}} for a quick overview of the \link{Bimap}
  objects and their interface).

  They are divided in 2 groups: (1) methods for getting or setting the
  direction of a \link{Bimap} object and (2) methods for getting, counting
  or setting the left or right keys (or mapped keys only) of a \link{Bimap}
  object. Note that all the methods in group (2) are undirected methods
  i.e. what they return does NOT depend on the direction of the map (more
  on this below).
}

\usage{
  ## Getting or setting the direction of a Bimap object
  direction(x)
  direction(x) <- value
  revmap(x, ...)

  ## Getting, counting or setting the left or right keys (or mapped
  ## keys only) of a Bimap object
  Lkeys(x)
  Rkeys(x)
  Llength(x)
  Rlength(x)
  mappedLkeys(x)
  mappedRkeys(x)
  count.mappedLkeys(x)
  count.mappedRkeys(x)
  Lkeys(x) <- value
  Rkeys(x) <- value
  subset(x, ...)
}

\arguments{
  \item{x}{
    A \link{Bimap} object.
  }
  \item{value}{
    A single integer or character string indicating the new direction
    in \code{direction(x) <- value}.
    A character vector containing the new keys (must be a subset of the
    current keys) in \code{Lkeys(x) <- value} and \code{Rkeys(x) <- value}.
  }
  \item{...}{
    Extra argument for \code{revmap} can be:
    \describe{
      \item{\code{objName}}{
        The name to give to the reversed map (only supported if \code{x} is an
        \link{AnnDbBimap} object).
      }
    }
    Extra arguments for \code{subset} can be:
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

\details{
  All \link{Bimap} objects have a direction which can be left-to-right
  (i.e. the mapping goes from the left keys to the right keys)
  or right-to-left (i.e. the mapping goes from the right keys to the
  left keys).
  A \link{Bimap} object \code{x} that maps from left to right is
  considered to be a direct map. Otherwise it is considered to be an
  indirect map (when it maps from right to left).

  \code{direction} returns \code{1} on a direct map and \code{-1}
  otherwise.

  The direction of \code{x} can be changed with \code{direction(x) <- value}
  where value must be \code{1} or \code{-1}.
  An easy way to reverse a map (i.e. to change its direction) is to
  do \code{direction(x) <- - direction(x)}, or, even better, to use
  \code{revmap(x)} which is actually the recommended way for doing it.

  The \code{Lkeys} and \code{Rkeys} methods return respectively
  the left and right keys of a \link{Bimap} object.
  Unlike the \code{\link{keys}} method (see \code{?\link{keys}} for
  more information), these methods are direction-independent i.e. what
  they return does NOT depend on the direction of the map.
  Such methods are also said to be "undirected methods" 
  and methods like the \code{\link{keys}} method are said
  to be "directed methods".

  All the methods described below are also "undirected methods".

  \code{Llength(x)} and \code{Rlength(x)} are equivalent to
  (but more efficient than) \code{length(Lkeys(x))} and
  \code{length(Rkeys(x))}, respectively.

  The \code{mappedLkeys} (or \code{mappedRkeys}) method returns
  the left keys (or right keys) that are mapped to at least one right
  key (or one left key).

  \code{count.mappedLkeys(x)} and \code{count.mappedRkeys(x)} are
  equivalent to (but more efficient than) \code{length(mappedLkeys(x))}
  and \code{length(mappedRkeys(x))}, respectively.

  \code{Lkeys(x) <- value} and \code{Rkeys(x) <- value} are the
  undirected versions of \code{keys(x) <- value} (see \code{?\link{keys}}
  for more information) and \code{subset(x, Lkeys=new_Lkeys, Rkeys=new_Rkeys)}
  is provided as a convenient way to reduce the sets of left
  and right keys in one single function call.
}

\value{
  \code{1L} or \code{-1L} for \code{direction}.

  A \link{Bimap} object of the same subtype as \code{x} for \code{revmap}
  and \code{subset}.

  A character vector for \code{Lkeys}, \code{Rkeys}, \code{mappedLkeys}
  and \code{mappedRkeys}.

  A single non-negative integer for \code{Llength}, \code{Rlength},
  \code{count.mappedLkeys} and \code{count.mappedRkeys}.
}

\author{H. Pages}

\seealso{
  \link{Bimap},
  \link{Bimap-keys},
  \link{BimapFormatting},
  \link{AnnDbBimap-envirAPI}
}

\examples{
  library(hgu95av2.db)
  ls(2)
  x <- hgu95av2GO
  x
  summary(x)
  direction(x)

  length(x)
  Llength(x)
  Rlength(x)

  keys(x)[1:4]
  Lkeys(x)[1:4]
  Rkeys(x)[1:4]

  count.mappedkeys(x)
  count.mappedLkeys(x)
  count.mappedRkeys(x)

  mappedkeys(x)[1:4]
  mappedLkeys(x)[1:4]
  mappedRkeys(x)[1:4]

  y <- revmap(x)
  y
  summary(y)
  direction(y)

  length(y)
  Llength(y)
  Rlength(y)

  keys(y)[1:4]
  Lkeys(y)[1:4]
  Rkeys(y)[1:4]

  ## etc...

  ## Get rid of all unmapped keys (left and right)
  z <- subset(y, Lkeys=mappedLkeys(y), Rkeys=mappedRkeys(y))
}

\keyword{methods}
