\name{pfamAC2INTERPRO}
\alias{pfamAC2INTERPRO}
\title{Mappings from Accession number to INTERPRO ID}
\description{
  Mapping function from Accession number (AC) to the associated INTERPRO ID (INTERPRO)
}
\usage{
pfamAC2INTERPRO(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of INTERPRO ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2INTERPRO will
  return a list of associated INTERPRO ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2INTERPRO(ac=NULL) (or pfamAC2INTERPRO())
  will return a list of associated INTERPRO ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: \url{http://www.ebi.ac.uk/interpro/}
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2INTERPRO <- pfamAC2INTERPRO()
    head(AC2INTERPRO)
    pfamAC2INTERPRO(ac=sample(names(AC2INTERPRO), 3))
}
}
\keyword{datasets}
