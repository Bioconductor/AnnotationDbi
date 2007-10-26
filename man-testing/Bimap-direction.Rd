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


\title{Methods for accessing/changing the direction of a Bimap object}

\description{
  These methods are part of the \link{Bimap} interface.
}

\usage{
  direction(x)
  direction(x) <- value
  revmap(x, ...)
}

\arguments{
  \item{x}{
    A \link{Bimap} object.
  }
  \item{value}{
    A single integer or character string indicating the new direction.
  }
  \item{...}{
    Extra argument can be:
    \describe{
      \item{\code{objName}}{
        The name to give to the reversed map (only supported if \code{x} is an
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
  Llength(x)
  Rlength(x)

  y <- revmap(x)
  y
  length(y)
  Llength(y)
  Rlength(y)
}

\keyword{methods}

