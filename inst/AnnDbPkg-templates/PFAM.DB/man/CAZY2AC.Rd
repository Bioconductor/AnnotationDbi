\name{pfamCAZY2AC}
\alias{pfamCAZY2AC}
\title{Mappings from CAZY ID to Accession number}
\description{
  Mapping function from CAZY ID (CAZY) to the associated Accession number (AC)
}
\usage{
pfamCAZY2AC(cazy=NULL)
}
\arguments{
  \item{cazy}{A character vector of CAZY ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{cazy} is a vector of CAZY ID,  pfamCAZY2AC will
  return a list of associated Accession number for the given \code{cazy}.  If
  \code{cazy} is NULL,  pfamCAZY2AC(cazy=NULL) (or pfamCAZY2AC())
  will return a list of associated Accession number for all the CAZY ID in
  the database.
  Extra information:  None
  More Details: The CAZy database (\url{http://afmb.cnrs-mrs.fr/CAZY/}) describes the families of structurally-related catalytic and carbohydrate-binding modules (or functional domains) of enzymes that degrade, modify, or create glycosidic bonds.
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    CAZY2AC <- pfamCAZY2AC()
    head(CAZY2AC)
    pfamCAZY2AC(cazy=sample(names(CAZY2AC), 3))
}
}
\keyword{datasets}
