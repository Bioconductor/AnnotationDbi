\name{pfamRM2AC}
\alias{pfamRM2AC}
\title{Mappings from Reference Medline to Accession number}
\description{
  Mapping function from Reference Medline (RM) to the associated Accession number (AC)
}
\usage{
pfamRM2AC(rm=NULL)
}
\arguments{
  \item{rm}{A character vector of Reference Medline or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{rm} is a vector of Reference Medline,  pfamRM2AC will
  return a list of associated Accession number for the given \code{rm}.  If
  \code{rm} is NULL,  pfamRM2AC(rm=NULL) (or pfamRM2AC())
  will return a list of associated Accession number for all the Reference Medline in
  the database.
  Extra information:  None
  More Details: Reference Medline (\url{http://www.ncbi.nlm.nih.gov/PubMed/})
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    RM2AC <- pfamRM2AC()
    head(RM2AC)
    pfamRM2AC(rm=sample(names(RM2AC), 3))
}
}
\keyword{datasets}
