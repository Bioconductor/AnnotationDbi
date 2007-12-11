\name{pfamID2AC}
\alias{pfamID2AC}
\title{Mappings from Identification to Accession number}
\description{
  Mapping function from Identification (ID) to the associated Accession number (AC)
}
\usage{
pfamID2AC(id=NULL)
}
\arguments{
  \item{id}{A character vector of Identification or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{id} is a vector of Identification,  pfamID2AC will
  return a list of associated Accession number for the given \code{id}.  If
  \code{id} is NULL,  pfamID2AC(id=NULL) (or pfamID2AC())
  will return a list of associated Accession number for all the Identification in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    ID2AC <- pfamID2AC()
    head(ID2AC)
    pfamID2AC(id=sample(names(ID2AC), 3))
}
}
\keyword{datasets}
