\name{pfamDE2AC}
\alias{pfamDE2AC}
\title{Mappings from Definition to Accession number}
\description{
  Mapping function from Definition (DE) to the associated Accession number (AC)
}
\usage{
pfamDE2AC(de=NULL)
}
\arguments{
  \item{de}{A character vector of Definition or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{de} is a vector of Definition,  pfamDE2AC will
  return a list of associated Accession number for the given \code{de}.  If
  \code{de} is NULL,  pfamDE2AC(de=NULL) (or pfamDE2AC())
  will return a list of associated Accession number for all the Definition in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    DE2AC <- pfamDE2AC()
    head(DE2AC)
    pfamDE2AC(de=sample(names(DE2AC), 3))
}
}
\keyword{datasets}
