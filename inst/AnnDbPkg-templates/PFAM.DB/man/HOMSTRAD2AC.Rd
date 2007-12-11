\name{pfamHOMSTRAD2AC}
\alias{pfamHOMSTRAD2AC}
\title{Mappings from HOMSTRAD ID to Accession number}
\description{
  Mapping function from HOMSTRAD ID (HOMSTRAD) to the associated Accession number (AC)
}
\usage{
pfamHOMSTRAD2AC(homstrad=NULL)
}
\arguments{
  \item{homstrad}{A character vector of HOMSTRAD ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{homstrad} is a vector of HOMSTRAD ID,  pfamHOMSTRAD2AC will
  return a list of associated Accession number for the given \code{homstrad}.  If
  \code{homstrad} is NULL,  pfamHOMSTRAD2AC(homstrad=NULL) (or pfamHOMSTRAD2AC())
  will return a list of associated Accession number for all the HOMSTRAD ID in
  the database.
  Extra information:  None
  More Details: HOMSTRAD (HOMologous STRucture Alignment Database, \url{http://www-cryst.bioc.cam.ac.uk/homstrad/}) is a curated database of structure-based alignments for homologous protein families.  Reference: Mizuguchi K, Deane CM, Blundell TL, Overington JP. (1998) HOMSTRAD: a database of protein structure alignments for homologous families. Protein Science 7:2469-2471.
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    HOMSTRAD2AC <- pfamHOMSTRAD2AC()
    head(HOMSTRAD2AC)
    pfamHOMSTRAD2AC(homstrad=sample(names(HOMSTRAD2AC), 3))
}
}
\keyword{datasets}
