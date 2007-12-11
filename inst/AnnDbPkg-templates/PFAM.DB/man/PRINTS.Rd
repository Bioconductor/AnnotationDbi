\name{pfamAC2PRINTS}
\alias{pfamAC2PRINTS}
\title{Mappings from Accession number to PRINTS ID}
\description{
  Mapping function from Accession number (AC) to the associated PRINTS ID (PRINTS)
}
\usage{
pfamAC2PRINTS(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of PRINTS ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2PRINTS will
  return a list of associated PRINTS ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2PRINTS(ac=NULL) (or pfamAC2PRINTS())
  will return a list of associated PRINTS ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: PRINTS (\url{http://umber.sbs.man.ac.uk/dbbrowser/PRINTS/}) is a compendium of protein fingerprints.
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2PRINTS <- pfamAC2PRINTS()
    head(AC2PRINTS)
    pfamAC2PRINTS(ac=sample(names(AC2PRINTS), 3))
}
}
\keyword{datasets}
