\name{pfamAC2RM}
\alias{pfamAC2RM}
\title{Mappings from Accession number to Reference Medline}
\description{
  Mapping function from Accession number (AC) to the associated Reference Medline (RM)
}
\usage{
pfamAC2RM(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of Reference Medline.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2RM will
  return a list of associated Reference Medline for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2RM(ac=NULL) (or pfamAC2RM())
  will return a list of associated Reference Medline for all the Accession number in
  the database.
  Extra information:  None
  More Details: Reference Medline (\url{http://www.ncbi.nlm.nih.gov/PubMed/})
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2RM <- pfamAC2RM()
    head(AC2RM)
    pfamAC2RM(ac=sample(names(AC2RM), 3))
}
}
\keyword{datasets}
