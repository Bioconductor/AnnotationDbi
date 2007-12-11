\name{pfamAC2LOAD}
\alias{pfamAC2LOAD}
\title{Mappings from Accession number to LOAD ID}
\description{
  Mapping function from Accession number (AC) to the associated LOAD ID (LOAD)
}
\usage{
pfamAC2LOAD(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of LOAD ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2LOAD will
  return a list of associated LOAD ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2LOAD(ac=NULL) (or pfamAC2LOAD())
  will return a list of associated LOAD ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2LOAD <- pfamAC2LOAD()
    head(AC2LOAD)
    pfamAC2LOAD(ac=sample(names(AC2LOAD), 3))
}
}
\keyword{datasets}
