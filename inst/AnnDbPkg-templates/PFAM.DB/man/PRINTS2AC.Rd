\name{pfamPRINTS2AC}
\alias{pfamPRINTS2AC}
\title{Mappings from PRINTS ID to Accession number}
\description{
  Mapping function from PRINTS ID (PRINTS) to the associated Accession number (AC)
}
\usage{
pfamPRINTS2AC(prints=NULL)
}
\arguments{
  \item{prints}{A character vector of PRINTS ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{prints} is a vector of PRINTS ID,  pfamPRINTS2AC will
  return a list of associated Accession number for the given \code{prints}.  If
  \code{prints} is NULL,  pfamPRINTS2AC(prints=NULL) (or pfamPRINTS2AC())
  will return a list of associated Accession number for all the PRINTS ID in
  the database.
  Extra information:  None
  More Details: PRINTS (\url{http://umber.sbs.man.ac.uk/dbbrowser/PRINTS/}) is a compendium of protein fingerprints.
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    PRINTS2AC <- pfamPRINTS2AC()
    head(PRINTS2AC)
    pfamPRINTS2AC(prints=sample(names(PRINTS2AC), 3))
}
}
\keyword{datasets}
