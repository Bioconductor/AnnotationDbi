\name{pfamSMART2AC}
\alias{pfamSMART2AC}
\title{Mappings from SMART ID to Accession number}
\description{
  Mapping function from SMART ID (SMART) to the associated Accession number (AC)
}
\usage{
pfamSMART2AC(smart=NULL)
}
\arguments{
  \item{smart}{A character vector of SMART ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{smart} is a vector of SMART ID,  pfamSMART2AC will
  return a list of associated Accession number for the given \code{smart}.  If
  \code{smart} is NULL,  pfamSMART2AC(smart=NULL) (or pfamSMART2AC())
  will return a list of associated Accession number for all the SMART ID in
  the database.
  Extra information:  None
  More Details: SMART (a Simple Modular Architecture Research Tool, \url{http://smart.embl-heidelberg.de/}) allows the identification and annotation of genetically mobile domains and the analysis of domain architectures.  Reference: (1) Schultz et al. (1998) Proc. Natl. Acad. Sci. USA 95, 5857-5864.  (2) Letunic et al. (2004) Nucleic Acids Res 32, D142-D144
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    SMART2AC <- pfamSMART2AC()
    head(SMART2AC)
    pfamSMART2AC(smart=sample(names(SMART2AC), 3))
}
}
\keyword{datasets}
