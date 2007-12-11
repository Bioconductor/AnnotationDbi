\name{pfamINTERPRO2AC}
\alias{pfamINTERPRO2AC}
\title{Mappings from INTERPRO ID to Accession number}
\description{
  Mapping function from INTERPRO ID (INTERPRO) to the associated Accession number (AC)
}
\usage{
pfamINTERPRO2AC(interpro=NULL)
}
\arguments{
  \item{interpro}{A character vector of INTERPRO ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{interpro} is a vector of INTERPRO ID,  pfamINTERPRO2AC will
  return a list of associated Accession number for the given \code{interpro}.  If
  \code{interpro} is NULL,  pfamINTERPRO2AC(interpro=NULL) (or pfamINTERPRO2AC())
  will return a list of associated Accession number for all the INTERPRO ID in
  the database.
  Extra information:  None
  More Details: \url{http://www.ebi.ac.uk/interpro/}
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    INTERPRO2AC <- pfamINTERPRO2AC()
    head(INTERPRO2AC)
    pfamINTERPRO2AC(interpro=sample(names(INTERPRO2AC), 3))
}
}
\keyword{datasets}
