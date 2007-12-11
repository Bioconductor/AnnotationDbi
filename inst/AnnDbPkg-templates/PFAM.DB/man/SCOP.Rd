\name{pfamAC2SCOP}
\alias{pfamAC2SCOP}
\title{Mappings from Accession number to SCOP ID}
\description{
  Mapping function from Accession number (AC) to the associated SCOP ID (SCOP)
}
\usage{
pfamAC2SCOP(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of SCOP ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2SCOP will
  return a list of associated SCOP ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2SCOP(ac=NULL) (or pfamAC2SCOP())
  will return a list of associated SCOP ID for all the Accession number in
  the database.
  Extra information:  Placement
  More Details: Structural Classification of Proteins (\url{http://scop.mrc-lmb.cam.ac.uk/scop/index.html}).  Reference: Murzin A. G., Brenner S. E., Hubbard T., Chothia C. (1995). SCOP: a structural classification of proteins database for the investigation of sequences and structures. J. Mol. Biol. 247, 536-540
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2SCOP <- pfamAC2SCOP()
    head(AC2SCOP)
    pfamAC2SCOP(ac=sample(names(AC2SCOP), 3))
}
}
\keyword{datasets}
