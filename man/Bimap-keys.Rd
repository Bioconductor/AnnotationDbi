\name{Bimap-keys}

\alias{Bimap-keys}

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

\alias{keys<-}
\alias{keys<-,Bimap-method}

\alias{[,Bimap-method}

\alias{show,AnnDbTable-method}


\title{Methods for manipulating the keys of a Bimap object}

\description{
  These methods are part of the \link{Bimap} interface
  (see \code{?\link{Bimap}} for a quick overview of the \link{Bimap}
  objects and their interface).
}

\usage{
  keys(x)
  #length(x)
  isNA(x)
  mappedkeys(x)
  count.mappedkeys(x)
  keys(x) <- value
  #x[i]
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
    A character vector containing the keys of the map elements to extract.
  }
}

\details{
  \code{keys(x)} returns the set of all valid keys for map \code{x}.
  For example, \code{keys(hgu95av2GO)} is the set of all probe set IDs
  for chip hgu95av2 from Affymetrix.
  Note that the double bracket operator \code{[[} for \link{Bimap}
  objects is guaranteed to work only with a valid key and will raise
  an error if the key is invalid.
  (See \code{?`\link{AnnDbBimap-envirAPI}`} for more information
  about this operator.)

  \code{length(x)} is equivalent to (but more efficient than)
  \code{length(keys(x))}.

  A valid key is not necessarily mapped (\code{[[} will return an
  \code{NA} on an unmapped key).

  \code{isNA(x)} returns a logical vector of the same length as \code{x}
  where the \code{TRUE} value is used to mark keys that are NOT mapped
  and the \code{FALSE} value to mark keys that ARE mapped. 

  \code{mappedkeys(x)} returns the subset of \code{keys(x)} where only
  mapped keys were kept.

  \code{count.mappedkeys(x)} is equivalent to (but more efficient than)
  \code{length(mappedkeys(x))}.
  
  Two (almost) equivalent forms of subsetting a \link{Bimap} object
  are provided: (1) by setting the keys explicitely and (2) by using
  the single bracket operator \code{[} for \link{Bimap} objects.
  Let's say the user wants to restrict the mapping to the subset of
  valid keys stored in character vector \code{mykeys}. This can be
  done either with \code{keys(x) <- mykeys} (form (1)) or with
  \code{y <- x[mykeys]} (form (2)).
  Please note that form (1) alters object \code{x} in an irreversible
  way (the original keys are lost) so form (2) should be preferred.

  All the methods described on this pages are "directed methods"
  i.e. what they return DOES depend on the direction of the \link{Bimap}
  object that they are applied to (see \code{?\link{direction}} for
  more information about this).
}

\value{
  A character vector for \code{keys} and \code{mappedkeys}.

  A single non-negative integer for \code{length} and
  \code{count.mappedkeys}.

  A logical vector for \code{isNA}.

  A \link{Bimap} object of the same subtype as \code{x} for \code{x[i]}.
}

\author{H. Pages}

\seealso{
  \link{Bimap},
  \link{AnnDbBimap-envirAPI},
  \link{Bimap-toTable},
  \link{BimapFormatting}
}

\examples{
  library(hgu95av2.db)
  x <- hgu95av2GO
  x
  length(x)
  count.mappedkeys(x)
  x[1:3]
  links(x[1:3])

  ## Keep only the mapped keys
  keys(x) <- mappedkeys(x)
  length(x)
  count.mappedkeys(x)
  x # now it is a submap

  ## The above subsetting can also be achieved with
  x <- hgu95av2GO[mappedkeys(hgu95av2GO)]

  ## mappedkeys() and count.mappedkeys() also work with an environment
  ## or a list
  z <- list(k1=NA, k2=letters[1:4], k3="x")
  mappedkeys(z)
  count.mappedkeys(z)
}

\keyword{methods}
