\name{AnnDbBimap-envirAPI}

\alias{AnnDbBimap-envirAPI}

\alias{ls}
\alias{ls,Bimap-method}

\alias{exists}
\alias{exists,ANY,ANY,Bimap-method}
\alias{exists,ANY,Bimap,missing-method}

\alias{get}
\alias{get,ANY,ANY,AnnDbBimap-method}
\alias{get,ANY,AnnDbBimap,missing-method}

\alias{[[,AnnDbBimap-method}

\alias{$,AnnDbBimap-method}

\alias{mget}
\alias{mget,AnnDbBimap-method}
\alias{mget,ANY,AnnDbBimap-method}

\alias{eapply}
\alias{eapply,Bimap-method}

\alias{contents,Bimap-method}

\alias{sample}
\alias{sample,Bimap-method}
\alias{sample,environment-method}


\title{Environment-like API for AnnDbBimap objects}

\description{
  These methods allow the user to manipulate any \link{AnnDbBimap} object as
  if it was an environment. This environment-like API is provided for
  backward compatibility with the traditional environment-based maps.
}

\usage{
  ls(name, pos, envir, all.names, pattern)
  exists(x, where, envir, frame, mode, inherits)
  get(x, pos, envir, mode, inherits)
  #x[[i]]
  #x$name

  ## Converting to a list
  mget(x, envir, mode, ifnotfound, inherits)
  eapply(env, FUN, ..., all.names)
  #contents(object, all.names)

  ## Additional convenience method
  sample(x, size, replace=FALSE, prob=NULL)
}

\arguments{
  \item{name}{
    An \link{AnnDbBimap} object for \code{ls}.
    A key as a literal character string or a name (possibly backtick quoted)
    for \code{x$name}.
  }
  \item{pos}{
    Ignored.
  }
  \item{envir}{
    Ignored for \code{ls}. An \link{AnnDbBimap} object for \code{mget},
    \code{get} and \code{exists}.
  }
  \item{all.names}{
    Ignored.
  }
  \item{pattern}{
    An optional regular expression. Only keys matching 'pattern' are returned.
  }
  \item{x}{
    The key(s) to search for for \code{exists}, \code{get} and \code{mget}.
    An \link{AnnDbBimap} object for \code{[[} and \code{x$name}.
    An \link{AnnDbBimap} object or an environment for \code{sample}.
  }
  \item{where}{
    Ignored.
  }
  \item{frame}{
    Ignored.
  }
  \item{mode}{
    Ignored.
  }
  \item{inherits}{
    Ignored.
  }
  \item{i}{
    Single key specifying the map element to extract.
  }
  \item{ifnotfound}{
    A value to be used if the key is not found. Only \code{NA} is currently
    supported.
  }
  \item{env}{
    An \link{AnnDbBimap} object.
  }
  \item{FUN}{
    The function to be applied (see original \code{\link[base]{eapply}}
    for environments for the details).
  }
  \item{...}{
    Optional arguments to \code{FUN}.
  }
  \item{size}{
    Non-negative integer giving the number of map elements to choose.
  }
  \item{replace}{
    Should sampling be with replacement?
  }
  \item{prob}{
    A vector of probability weights for obtaining the elements of the map
    being sampled.
  }
}

\value{
}

\seealso{
  \code{\link[base]{ls}},
  \code{\link[base]{exists}},
  \code{\link[base]{get}},
  \code{\link[base]{mget}},
  \code{\link[base]{eapply}},
  \code{\link[Biobase]{contents}},
  \code{\link[base]{sample}},
  \link{Bimap}
}

\examples{
  library(hgu95av2.db)
  x <- hgu95av2CHRLOC

  ls(x)[1:3]
  exists(ls(x)[1], x)
  exists("titi", x)
  get(ls(x)[1], x)
  x[[ls(x)[1]]]
  x$titi # NULL

  mget(ls(x)[1:3], x)
  eapply(x, length)
  contents(x)

  sample(x, 3)
}

\keyword{methods}
\keyword{interface}

