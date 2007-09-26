\name{BimapFormatting}

\alias{BimapFormatting}

\alias{as.list}
\alias{as.list,Bimap-method}
\alias{as.list,AnnDbBimap-method}
\alias{as.list,IpiAnnDbMap-method}
\alias{as.list,AgiAnnDbMap-method}
\alias{as.list,GoAnnDbBimap-method}
\alias{as.list,GOTermsAnnDbBimap-method}

\alias{as.character,AnnDbBimap-method}


\title{Formatting a Bimap as a list or character vector}

\description{
}

\usage{
  ## Formatting as a list
  as.list(x, ...)

  ## Formatting as a character vector
  #as.character(x, ...)

}

\arguments{
  \item{x}{
    A \link{Bimap} object.
  }
  \item{...}{
    Further arguments are ignored.
  }
}

\value{
}

\author{H. Pages}

\seealso{
  \link{Bimap},
  \link{AnnDbBimap-envirAPI}
}

\examples{
  library(hgu95av2.db)
  as.list(hgu95av2CHRLOC)[1:9]
  as.list(hgu95av2ENTREZID)[1:9]
  as.character(hgu95av2ENTREZID)[1:9]
}

\keyword{methods}
