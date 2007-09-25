\name{AnnDbObj-objects}
\docType{class}

\alias{AnnDbObj-objects}

% Classes
\alias{AnnDbObj}
\alias{class:AnnDbObj}
\alias{AnnDbObj-class}

% Accessor-like methods

\alias{dbconn}
\alias{dbconn,AnnDbObj-method}
\alias{dbfile}
\alias{dbfile,AnnDbObj-method}


\title{AnnDbObj objects}

\description{
  The AnnDbObj class is the most general container for storing any kind
  of SQLite-based annotation data.
}

\details{
  Many classes in AnnotationDbi inherit directly or indirectly
  from the AnnDbObj class. One important particular case is the
  \link{AnnDbBimap} class which is the lowest class in the AnnDbObj
  hierarchy to also inherit the \link{Bimap} interface.
}

\section{Accessor-like methods}{
  In the code snippet below,
  \code{x} is an AnnDbObj object.

  \describe{
    \item{}{
      \code{dbconn(x)}:
      Return a connection object to the SQLite DB containing \code{x}'s data.
    }
  }
  \describe{
    \item{}{
      \code{dbfile(x)}:
      Return the path (character string) to the SQLite DB (file) containing
      \code{x}'s data.
    }
  }
}

\seealso{
  \link{Bimap}
}

\examples{
  library(hgu95av2.db)
  dbconn(hgu95av2GO)
  dbfile(hgu95av2GO)
}

\keyword{classes}
\keyword{methods}

