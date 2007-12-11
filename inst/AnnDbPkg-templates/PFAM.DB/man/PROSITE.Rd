\name{pfamAC2PROSITE}
\alias{pfamAC2PROSITE}
\title{Mappings from Accession number to PROSITE ID}
\description{
  Mapping function from Accession number (AC) to the associated PROSITE ID (PROSITE)
}
\usage{
pfamAC2PROSITE(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of PROSITE ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2PROSITE will
  return a list of associated PROSITE ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2PROSITE(ac=NULL) (or pfamAC2PROSITE())
  will return a list of associated PROSITE ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: PROSITE (\url{http://www.expasy.ch/prosite/}) is a database of protein families and domains. It consists of biologically significant sites, patterns and profiles that help to reliably identify to which known protein family (if any) a new sequence belongs.  Reference: Hulo N., Sigrist C.J.A., Le Saux V., Langendijk-Genevaux P.S., Bordoli L., Gattiker A., De Castro E., Bucher P., Bairoch A. Recent improvements to the PROSITE database. Nucl. Acids. Res. 32:D134-D137(2004)
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2PROSITE <- pfamAC2PROSITE()
    head(AC2PROSITE)
    pfamAC2PROSITE(ac=sample(names(AC2PROSITE), 3))
}
}
\keyword{datasets}
