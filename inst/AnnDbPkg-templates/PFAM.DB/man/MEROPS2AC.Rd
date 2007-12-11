\name{pfamMEROPS2AC}
\alias{pfamMEROPS2AC}
\title{Mappings from MEROPS ID to Accession number}
\description{
  Mapping function from MEROPS ID (MEROPS) to the associated Accession number (AC)
}
\usage{
pfamMEROPS2AC(merops=NULL)
}
\arguments{
  \item{merops}{A character vector of MEROPS ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{merops} is a vector of MEROPS ID,  pfamMEROPS2AC will
  return a list of associated Accession number for the given \code{merops}.  If
  \code{merops} is NULL,  pfamMEROPS2AC(merops=NULL) (or pfamMEROPS2AC())
  will return a list of associated Accession number for all the MEROPS ID in
  the database.
  Extra information:  None
  More Details: The MEROPS database (\url{http://merops.sanger.ac.uk/}) is an information resource for peptidases (also termed proteases, proteinases and proteolytic enzymes) and the proteins that inhibit them.  Reference: Rawlings, N.D., Tolle, D.P. & Barrett, A.J. (2004) MEROPS: the peptidase database. Nucleic Acids Res. 32 Database issue, D160-D164
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    MEROPS2AC <- pfamMEROPS2AC()
    head(MEROPS2AC)
    pfamMEROPS2AC(merops=sample(names(MEROPS2AC), 3))
}
}
\keyword{datasets}
