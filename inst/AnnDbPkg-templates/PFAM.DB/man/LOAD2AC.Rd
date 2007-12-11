\name{pfamLOAD2AC}
\alias{pfamLOAD2AC}
\title{Mappings from LOAD ID to Accession number}
\description{
  Mapping function from LOAD ID (LOAD) to the associated Accession number (AC)
}
\usage{
pfamLOAD2AC(load=NULL)
}
\arguments{
  \item{load}{A character vector of LOAD ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{load} is a vector of LOAD ID,  pfamLOAD2AC will
  return a list of associated Accession number for the given \code{load}.  If
  \code{load} is NULL,  pfamLOAD2AC(load=NULL) (or pfamLOAD2AC())
  will return a list of associated Accession number for all the LOAD ID in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    LOAD2AC <- pfamLOAD2AC()
    head(LOAD2AC)
    pfamLOAD2AC(load=sample(names(LOAD2AC), 3))
}
}
\keyword{datasets}
