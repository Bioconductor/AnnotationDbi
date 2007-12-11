\name{pfamAC2HOMSTRAD}
\alias{pfamAC2HOMSTRAD}
\title{Mappings from Accession number to HOMSTRAD ID}
\description{
  Mapping function from Accession number (AC) to the associated HOMSTRAD ID (HOMSTRAD)
}
\usage{
pfamAC2HOMSTRAD(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of HOMSTRAD ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2HOMSTRAD will
  return a list of associated HOMSTRAD ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2HOMSTRAD(ac=NULL) (or pfamAC2HOMSTRAD())
  will return a list of associated HOMSTRAD ID for all the Accession number in
  the database.
  Extra information:  None
  More Details: HOMSTRAD (HOMologous STRucture Alignment Database, \url{http://www-cryst.bioc.cam.ac.uk/homstrad/}) is a curated database of structure-based alignments for homologous protein families.  Reference: Mizuguchi K, Deane CM, Blundell TL, Overington JP. (1998) HOMSTRAD: a database of protein structure alignments for homologous families. Protein Science 7:2469-2471.
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2HOMSTRAD <- pfamAC2HOMSTRAD()
    head(AC2HOMSTRAD)
    pfamAC2HOMSTRAD(ac=sample(names(AC2HOMSTRAD), 3))
}
}
\keyword{datasets}
