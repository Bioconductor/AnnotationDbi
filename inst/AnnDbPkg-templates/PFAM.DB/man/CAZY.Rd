\name{pfamAC2CAZY}
\alias{pfamAC2CAZY}
\title{Mappings from Accession number to CAZY ID}
\description{
  Mapping function from Accession number (AC) to the associated CAZY ID (CAZY)
}
\usage{
pfamAC2CAZY(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of CAZY ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2CAZY will
  return a list of associated CAZY ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2CAZY(ac=NULL) (or pfamAC2CAZY())
  will return a list of associated CAZY ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: The CAZy database (\url{http://afmb.cnrs-mrs.fr/CAZY/}) describes the families of structurally-related catalytic and carbohydrate-binding modules (or functional domains) of enzymes that degrade, modify, or create glycosidic bonds.
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2CAZY <- pfamAC2CAZY()
    head(AC2CAZY)
    pfamAC2CAZY(ac=sample(names(AC2CAZY), 3))
}
}
\keyword{datasets}
