\name{pfamAC2MEROPS}
\alias{pfamAC2MEROPS}
\title{Mappings from Accession number to MEROPS ID}
\description{
  Mapping function from Accession number (AC) to the associated MEROPS ID (MEROPS)
}
\usage{
pfamAC2MEROPS(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of MEROPS ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2MEROPS will
  return a list of associated MEROPS ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2MEROPS(ac=NULL) (or pfamAC2MEROPS())
  will return a list of associated MEROPS ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: The MEROPS database (\url{http://merops.sanger.ac.uk/}) is an information resource for peptidases (also termed proteases, proteinases and proteolytic enzymes) and the proteins that inhibit them.  Reference: Rawlings, N.D., Tolle, D.P. & Barrett, A.J. (2004) MEROPS: the peptidase database. Nucleic Acids Res. 32 Database issue, D160-D164
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2MEROPS <- pfamAC2MEROPS()
    head(AC2MEROPS)
    pfamAC2MEROPS(ac=sample(names(AC2MEROPS), 3))
}
}
\keyword{datasets}
