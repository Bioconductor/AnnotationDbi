\name{pfamAC2TP}
\alias{pfamAC2TP}
\title{Mappings from Accession number to Type field}
\description{
  Mapping function from Accession number (AC) to the associated Type field (TP)
}
\usage{
pfamAC2TP(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of Type field.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2TP will
  return a list of associated Type field for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2TP(ac=NULL) (or pfamAC2TP())
  will return a list of associated Type field for all the Accession number in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2TP <- pfamAC2TP()
    head(AC2TP)
    pfamAC2TP(ac=sample(names(AC2TP), 3))
}
}
\keyword{datasets}
