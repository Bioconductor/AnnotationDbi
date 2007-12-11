\name{pfamAC2DE}
\alias{pfamAC2DE}
\title{Mappings from Accession number to Definition}
\description{
  Mapping function from Accession number (AC) to the associated Definition (DE)
}
\usage{
pfamAC2DE(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of Definition.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2DE will
  return a list of associated Definition for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2DE(ac=NULL) (or pfamAC2DE())
  will return a list of associated Definition for all the Accession number in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2DE <- pfamAC2DE()
    head(AC2DE)
    pfamAC2DE(ac=sample(names(AC2DE), 3))
}
}
\keyword{datasets}
