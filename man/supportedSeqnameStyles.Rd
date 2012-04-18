\name{supportedSeqnameStyles}

\alias{supportedSeqnameStyles}

\title{List the supported seqname styles for all supported organisms}

\description{
  Lists out the available seqname styles for all organisms supported by
  the seqnames.db database.  
}

\usage{
  supportedSeqnameStyles()
}


\details{
  Different organizations have different naming conventions for how they
  name the biologically defined sequence elements (usually chromosomes)
  for each organism they support.  The seqnames.db package contains a
  database that defines these different conventions.  The
  \code{supportedSeqnameStyles} function allows users to discover
  which conventions are currently encoded by the seqnames DB.
}

\value{
  A character list containing character vectors with the names of all
  seqname styles for each supported species.  The names of the list
  are the species names for each supported organism.
}

\author{H. Pages and Marc Carlson}

\examples{
  supportedSeqnameStyles()
}

\keyword{manip}
