\name{pfamAC2URL}
\alias{pfamAC2URL}
\title{Mappings from Accession number to URL ID}
\description{
  Mapping function from Accession number (AC) to the associated URL ID (URL)
}
\usage{
pfamAC2URL(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of URL ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2URL will
  return a list of associated URL ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2URL(ac=NULL) (or pfamAC2URL())
  will return a list of associated URL ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2URL <- pfamAC2URL()
    head(AC2URL)
    pfamAC2URL(ac=sample(names(AC2URL), 3))
}
}
\keyword{datasets}
