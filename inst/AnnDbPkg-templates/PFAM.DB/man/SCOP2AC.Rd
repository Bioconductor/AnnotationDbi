\name{pfamSCOP2AC}
\alias{pfamSCOP2AC}
\title{Mappings from SCOP ID to Accession number}
\description{
  Mapping function from SCOP ID (SCOP) to the associated Accession number (AC)
}
\usage{
pfamSCOP2AC(scop=NULL)
}
\arguments{
  \item{scop}{A character vector of SCOP ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{scop} is a vector of SCOP ID,  pfamSCOP2AC will
  return a list of associated Accession number for the given \code{scop}.  If
  \code{scop} is NULL,  pfamSCOP2AC(scop=NULL) (or pfamSCOP2AC())
  will return a list of associated Accession number for all the SCOP ID in
  the database.
  Extra information:  None
  More Details: Structural Classification of Proteins (\url{http://scop.mrc-lmb.cam.ac.uk/scop/index.html}).  Reference: Murzin A. G., Brenner S. E., Hubbard T., Chothia C. (1995). SCOP: a structural classification of proteins database for the investigation of sequences and structures. J. Mol. Biol. 247, 536-540
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    SCOP2AC <- pfamSCOP2AC()
    head(SCOP2AC)
    pfamSCOP2AC(scop=sample(names(SCOP2AC), 3))
}
}
\keyword{datasets}
