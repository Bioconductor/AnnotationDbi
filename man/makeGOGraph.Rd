\name{makeGOGraph}

\alias{makeGOGraph}

\title{A convenience function to generate graphs based on the GO.db package}

\description{
  \code{makeGOGraph} is a function to quickly convert any of the three Gene
  Ontologies in GO.db into a graphNEL object where each edge is given a
  weight of 1.
}

\usage{
  makeGOGraph(ont = c("bp","mf","cc"))
}

\arguments{
  \item{ont}{Specifies the ontology: "cc", "bp" or "mf".}
}

\author{Marc Carlson}

\seealso{
  \code{\link{GOTerms}}
}

\examples{
  ## makes a GO graph from the CC ontology 
  f <- makeGOGraph("cc")
}

\keyword{utilities}
\keyword{manip}


