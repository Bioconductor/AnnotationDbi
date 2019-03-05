\name{make_eg_to_go_map}
\alias{make_eg_to_go_map}
\alias{make_go_to_eg_map}

\title{Create GO to Entrez Gene maps for chip-based packages}
\description{
  Create a new map object mapping Entrez ID to GO (or vice versa) given
  a chip annotation data package.

  This is a temporary solution until a more general pluggable map
  solution comes online.
}
\usage{
make_eg_to_go_map(chip)
}

\arguments{
  \item{chip}{The name of the annotation data package.}
}

\value{
  Either a \code{Go3AnnDbMap} or a \code{RevGo3AnnDbMap}.
}
\author{Seth Falcon and Hervé Pagès}
\examples{
library("hgu95av2.db")

eg2go = make_eg_to_go_map("hgu95av2.db")
sample(eg2go, 2)

go2eg = make_go_to_eg_map("hgu95av2.db")
sample(go2eg, 2)

}
