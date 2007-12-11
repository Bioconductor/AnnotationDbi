\name{pfamAC2PFAMB}
\alias{pfamAC2PFAMB}
\title{Mappings from Accession number to PFAMB ID}
\description{
  Mapping function from Accession number (AC) to the associated PFAMB ID (PFAMB)
}
\usage{
pfamAC2PFAMB(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of PFAMB ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2PFAMB will
  return a list of associated PFAMB ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2PFAMB(ac=NULL) (or pfamAC2PFAMB())
  will return a list of associated PFAMB ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: Pfam-B is an automatic clustering of the rest of SWISSPROT and TrEMBL derived from the PRODOM database.
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2PFAMB <- pfamAC2PFAMB()
    head(AC2PFAMB)
    pfamAC2PFAMB(ac=sample(names(AC2PFAMB), 3))
}
}
\keyword{datasets}
