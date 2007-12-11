\name{pfamPROSITEPROFILE2AC}
\alias{pfamPROSITEPROFILE2AC}
\title{Mappings from PROSITE\_PROFILE ID to Accession number}
\description{
  Mapping function from PROSITE\_PROFILE ID (PROSITEPROFILE) to the associated Accession number (AC)
}
\usage{
pfamPROSITEPROFILE2AC(prositeprofile=NULL)
}
\arguments{
  \item{prositeprofile}{A character vector of PROSITE\_PROFILE ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{prositeprofile} is a vector of PROSITE\_PROFILE ID,  pfamPROSITEPROFILE2AC will
  return a list of associated Accession number for the given \code{prositeprofile}.  If
  \code{prositeprofile} is NULL,  pfamPROSITEPROFILE2AC(prositeprofile=NULL) (or pfamPROSITEPROFILE2AC())
  will return a list of associated Accession number for all the PROSITE\_PROFILE ID in
  the database.
  Extra information:  None
  More Details: None
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    PROSITEPROFILE2AC <- pfamPROSITEPROFILE2AC()
    head(PROSITEPROFILE2AC)
    pfamPROSITEPROFILE2AC(prositeprofile=sample(names(PROSITEPROFILE2AC), 3))
}
}
\keyword{datasets}
