\name{Bimap-colmethods}

\alias{Bimap-colmethods}

% Classes

\alias{Bimap}
\alias{class:Bimap}
\alias{Bimap-class}

% Methods

\alias{colnames}
\alias{colnames,FlatBimap-method}
\alias{colnames,AnnDbBimap-method}

\alias{colmetanames}
\alias{colmetanames,FlatBimap-method}
\alias{colmetanames,AnnDbBimap-method}

\alias{Lkeyname}
\alias{Lkeyname,Bimap-method}
\alias{Lkeyname,AnnDbBimap-method}
\alias{Rkeyname}
\alias{Rkeyname,Bimap-method}
\alias{Rkeyname,AnnDbBimap-method}
\alias{keyname}
\alias{keyname,Bimap-method}

\alias{tagname}
\alias{tagname,Bimap-method}
\alias{tagname,AnnDbBimap-method}

\alias{Rattribnames}
\alias{Rattribnames,Bimap-method}
\alias{Rattribnames,AnnDbBimap-method}

\alias{Rattribnames<-}
\alias{Rattribnames<-,Bimap-method}
\alias{Rattribnames<-,AnnDbBimap-method}

\alias{ncol}
\alias{ncol,Bimap-method}


\title{Methods for manipulating the col names of a Bimap object}

\description{
  Accessor methods for manipulating the col names of a Bimap object.

  Note that all the methods below: (1) are undirected Bimap methods
  (i.e. their result does NOT depend on the \link{direction}
  of the map) and (2) don't access the database.
}

\usage{
  colnames(x, do.NULL=TRUE, prefix="col")
  colmetanames(x)
  Lkeyname(x)
  Rkeyname(x)
  keyname(x)
  tagname(x)
  Rattribnames(x)
  Rattribnames(x) <- value
  ncol(x)
}

\arguments{
  \item{x}{
    A Bimap object.
  }
  \item{do.NULL}{
    Ignored.
  }
  \item{prefix}{
    Ignored.
  }
  \item{value}{
    A character vector containing the new right-attribute names
    (must be a subset of the current right-attribute names)
    or NULL.
  }
}

\value{
}

\author{H. Pages}

\seealso{
  \link{Bimap-keymethods},
  \code{\link{direction}},
  \link{AnnDbBimap-envirAPI}
}

\examples{
  library(hgu95av2.db)
  x <- hgu95av2GO
  y <- hgu95av2GO2PROBE
  colnames(x)
  colnames(y)
  colmetanames(x)
  colmetanames(y)
  Lkeyname(x)
  Lkeyname(y)
  Rkeyname(x)
  Rkeyname(y)
  keyname(x)
  keyname(y)
  tagname(x)
  tagname(y)
  Rattribnames(x)
  Rattribnames(y)
  ncol(x)
  ncol(y)
}

\keyword{methods}

