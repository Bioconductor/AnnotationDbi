\name{pfamTP2AC}
\alias{pfamTP2AC}
\title{Mappings from Type field to Accession number}
\description{
  Mapping function from Type field (TP) to the associated Accession number (AC)
}
\usage{
pfamTP2AC(tp=NULL)
}
\arguments{
  \item{tp}{A character vector of Type field or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{tp} is a vector of Type field,  pfamTP2AC will
  return a list of associated Accession number for the given \code{tp}.  If
  \code{tp} is NULL,  pfamTP2AC(tp=NULL) (or pfamTP2AC())
  will return a list of associated Accession number for all the Type field in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    TP2AC <- pfamTP2AC()
    head(TP2AC)
    pfamTP2AC(tp=sample(names(TP2AC), 3))
}
}
\keyword{datasets}
