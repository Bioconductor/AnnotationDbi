\name{AnnotationDb-objects}
\docType{class}

% Classes
\alias{AnnotationDb}
\alias{class:AnnotationDb}
\alias{AnnotationDb-class}

% Accessor-like methods
\alias{loadDb}
\alias{loadDb,character,character,character-method}
\alias{loadDb,character,missing,missing-method}
\alias{dbConn,AnnotationDb-method}
\alias{cols}
\alias{cols,AnnotationDb-method}
\alias{keys,AnnotationDb-method}
\alias{metadata,AnnotationDb-method}
\alias{names,AnnotationDb-method}
\alias{select}
\alias{select,AnnotationDb,character,character-method}
\alias{select,AnnotationDb,character,missing-method}
\alias{select,AnnotationDb,missing,character-method}
\alias{select,AnnotationDb,missing,missing-method}
\alias{saveDb}
\alias{saveDb,AnnotationDb-method}
\alias{show,AnnotationDb-method}


\title{AnnotationDb objects}

\description{
  AnnotaionDb is the virtual base class for all annotation packages.
}

\details{
}

\seealso{
  \code{\link[DBI:dbConnect]{dbConnect}},
  \code{\link[DBI:dbListTables]{dbListTables}},
  \code{\link[DBI:dbListTables]{dbListFields}},
  \code{\link[DBI:dbSendQuery]{dbGetQuery}},
  \link{Bimap}
}

\examples{
}

\keyword{classes}
\keyword{methods}

