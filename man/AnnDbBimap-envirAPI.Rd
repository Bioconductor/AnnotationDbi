\name{AnnDbBimap-envirAPI}

\alias{AnnDbBimap-envirAPI}

\alias{ls}
\alias{ls,AnnDbBimap-method}

\alias{exists}
\alias{exists,ANY,ANY,AnnDbBimap-method}
\alias{exists,ANY,AnnDbBimap,missing-method}

\alias{get}
\alias{get,ANY,ANY,AnnDbBimap-method}
\alias{get,ANY,AnnDbBimap,missing-method}

\alias{[[,AnnDbBimap-method}

\alias{$,AnnDbBimap-method}

\alias{mget}
\alias{mget,AnnDbBimap-method}
\alias{mget,ANY,AnnDbBimap-method}

\alias{as.list}
\alias{as.list,AnnDbBimap-method}
\alias{as.list,IpiAnnDbMap-method}
\alias{as.list,AgiAnnDbMap-method}
\alias{as.list,GoAnnDbBimap-method}
\alias{as.list,GONodeAnnDbBimap-method}

\alias{eapply}
\alias{eapply,AnnDbBimap-method}

\alias{sample}
\alias{sample,AnnDbBimap-method}
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
  as.list(x, ...)
  eapply(env, FUN, ..., all.names)

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
    An \link{AnnDbBimap} object for \code{[[}, \code{x$name} and \code{as.list}.
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
    Optional arguments to \code{FUN}. These arguments are ignored by \code{as.list}.
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
  \link{Bimap}
}

\examples{
  library(hgu95av2.db)
  x <- hgu95av2CHRLOC
  ls(x)[1:3]
  mget(ls(x)[1:3], x)
  x[["titi"]]
  sample(x, 3)
}

\keyword{methods}
\keyword{interface}

