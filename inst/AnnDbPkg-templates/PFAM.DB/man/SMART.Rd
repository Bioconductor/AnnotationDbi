\name{pfamAC2SMART}
\alias{pfamAC2SMART}
\title{Mappings from Accession number to SMART ID}
\description{
  Mapping function from Accession number (AC) to the associated SMART ID (SMART)
}
\usage{
pfamAC2SMART(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of SMART ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2SMART will
  return a list of associated SMART ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2SMART(ac=NULL) (or pfamAC2SMART())
  will return a list of associated SMART ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: SMART (a Simple Modular Architecture Research Tool, \url{http://smart.embl-heidelberg.de/}) allows the identification and annotation of genetically mobile domains and the analysis of domain architectures.  Reference: (1) Schultz et al. (1998) Proc. Natl. Acad. Sci. USA 95, 5857-5864.  (2) Letunic et al. (2004) Nucleic Acids Res 32, D142-D144
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2SMART <- pfamAC2SMART()
    head(AC2SMART)
    pfamAC2SMART(ac=sample(names(AC2SMART), 3))
}
}
\keyword{datasets}
