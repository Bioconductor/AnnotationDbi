\name{pfamURL2AC}
\alias{pfamURL2AC}
\title{Mappings from URL ID to Accession number}
\description{
  Mapping function from URL ID (URL) to the associated Accession number (AC)
}
\usage{
pfamURL2AC(url=NULL)
}
\arguments{
  \item{url}{A character vector of URL ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{url} is a vector of URL ID,  pfamURL2AC will
  return a list of associated Accession number for the given \code{url}.  If
  \code{url} is NULL,  pfamURL2AC(url=NULL) (or pfamURL2AC())
  will return a list of associated Accession number for all the URL ID in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    URL2AC <- pfamURL2AC()
    head(URL2AC)
    pfamURL2AC(url=sample(names(URL2AC), 3))
}
}
\keyword{datasets}
