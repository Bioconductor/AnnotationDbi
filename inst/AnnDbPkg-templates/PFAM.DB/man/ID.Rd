\name{pfamAC2ID}
\alias{pfamAC2ID}
\title{Mappings from Accession number to Identification}
\description{
  Mapping function from Accession number (AC) to the associated Identification (ID)
}
\usage{
pfamAC2ID(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of Identification.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2ID will
  return a list of associated Identification for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2ID(ac=NULL) (or pfamAC2ID())
  will return a list of associated Identification for all the Accession number in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2ID <- pfamAC2ID()
    head(AC2ID)
    pfamAC2ID(ac=sample(names(AC2ID), 3))
}
}
\keyword{datasets}
