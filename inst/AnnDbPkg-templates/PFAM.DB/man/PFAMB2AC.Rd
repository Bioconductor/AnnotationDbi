\name{pfamPFAMB2AC}
\alias{pfamPFAMB2AC}
\title{Mappings from PFAMB ID to Accession number}
\description{
  Mapping function from PFAMB ID (PFAMB) to the associated Accession number (AC)
}
\usage{
pfamPFAMB2AC(pfamb=NULL)
}
\arguments{
  \item{pfamb}{A character vector of PFAMB ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{pfamb} is a vector of PFAMB ID,  pfamPFAMB2AC will
  return a list of associated Accession number for the given \code{pfamb}.  If
  \code{pfamb} is NULL,  pfamPFAMB2AC(pfamb=NULL) (or pfamPFAMB2AC())
  will return a list of associated Accession number for all the PFAMB ID in
  the database.
  Extra information:  None
  More Details: Pfam-B is an automatic clustering of the rest of SWISSPROT and TrEMBL derived from the PRODOM database.
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    PFAMB2AC <- pfamPFAMB2AC()
    head(PFAMB2AC)
    pfamPFAMB2AC(pfamb=sample(names(PFAMB2AC), 3))
}
}
\keyword{datasets}
