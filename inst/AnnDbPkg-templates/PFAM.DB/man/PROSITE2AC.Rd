\name{pfamPROSITE2AC}
\alias{pfamPROSITE2AC}
\title{Mappings from PROSITE ID to Accession number}
\description{
  Mapping function from PROSITE ID (PROSITE) to the associated Accession number (AC)
}
\usage{
pfamPROSITE2AC(prosite=NULL)
}
\arguments{
  \item{prosite}{A character vector of PROSITE ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{prosite} is a vector of PROSITE ID,  pfamPROSITE2AC will
  return a list of associated Accession number for the given \code{prosite}.  If
  \code{prosite} is NULL,  pfamPROSITE2AC(prosite=NULL) (or pfamPROSITE2AC())
  will return a list of associated Accession number for all the PROSITE ID in
  the database.
  Extra information:  None
  More Details: PROSITE (\url{http://www.expasy.ch/prosite/}) is a database of protein families and domains. It consists of biologically significant sites, patterns and profiles that help to reliably identify to which known protein family (if any) a new sequence belongs.  Reference: Hulo N., Sigrist C.J.A., Le Saux V., Langendijk-Genevaux P.S., Bordoli L., Gattiker A., De Castro E., Bucher P., Bairoch A. Recent improvements to the PROSITE database. Nucl. Acids. Res. 32:D134-D137(2004)
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    PROSITE2AC <- pfamPROSITE2AC()
    head(PROSITE2AC)
    pfamPROSITE2AC(prosite=sample(names(PROSITE2AC), 3))
}
}
\keyword{datasets}
