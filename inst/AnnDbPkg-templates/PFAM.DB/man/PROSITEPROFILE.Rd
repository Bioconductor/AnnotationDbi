\name{pfamAC2PROSITEPROFILE}
\alias{pfamAC2PROSITEPROFILE}
\title{Mappings from Accession number to PROSITE\_PROFILE ID}
\description{
  Mapping function from Accession number (AC) to the associated PROSITE\_PROFILE ID (PROSITEPROFILE)
}
\usage{
pfamAC2PROSITEPROFILE(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of PROSITE\_PROFILE ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2PROSITEPROFILE will
  return a list of associated PROSITE\_PROFILE ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2PROSITEPROFILE(ac=NULL) (or pfamAC2PROSITEPROFILE())
  will return a list of associated PROSITE\_PROFILE ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2PROSITEPROFILE <- pfamAC2PROSITEPROFILE()
    head(AC2PROSITEPROFILE)
    pfamAC2PROSITEPROFILE(ac=sample(names(AC2PROSITEPROFILE), 3))
}
}
\keyword{datasets}
