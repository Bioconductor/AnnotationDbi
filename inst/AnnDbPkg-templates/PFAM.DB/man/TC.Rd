\name{pfamAC2TC}
\alias{pfamAC2TC}
\title{Mappings from Accession number to Trusted cutoff}
\description{
  Mapping function from Accession number (AC) to the associated Trusted cutoff (TC)
}
\usage{
pfamAC2TC(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of Trusted cutoff.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2TC will
  return a list of associated Trusted cutoff for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2TC(ac=NULL) (or pfamAC2TC())
  will return a list of associated Trusted cutoff for all the Accession number in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2TC <- pfamAC2TC()
    head(AC2TC)
    pfamAC2TC(ac=sample(names(AC2TC), 3))
}
}
\keyword{datasets}
