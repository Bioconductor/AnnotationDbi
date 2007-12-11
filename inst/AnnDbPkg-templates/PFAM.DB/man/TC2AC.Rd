\name{pfamTC2AC}
\alias{pfamTC2AC}
\title{Mappings from Trusted cutoff to Accession number}
\description{
  Mapping function from Trusted cutoff (TC) to the associated Accession number (AC)
}
\usage{
pfamTC2AC(tc=NULL)
}
\arguments{
  \item{tc}{A character vector of Trusted cutoff or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{tc} is a vector of Trusted cutoff,  pfamTC2AC will
  return a list of associated Accession number for the given \code{tc}.  If
  \code{tc} is NULL,  pfamTC2AC(tc=NULL) (or pfamTC2AC())
  will return a list of associated Accession number for all the Trusted cutoff in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    TC2AC <- pfamTC2AC()
    head(TC2AC)
    pfamTC2AC(tc=sample(names(TC2AC), 3))
}
}
\keyword{datasets}
