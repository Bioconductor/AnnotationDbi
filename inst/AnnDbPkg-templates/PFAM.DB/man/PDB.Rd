\name{pfamAC2PDB}
\alias{pfamAC2PDB}
\title{Mappings from Accession number to PDB ID}
\description{
  Mapping function from Accession number (AC) to the associated PDB ID (PDB)
}
\usage{
pfamAC2PDB(ac=NULL)
}
\arguments{
  \item{ac}{A character vector of Accession number or NULL (default)}
}
\value{
  A list of PDB ID.
}
\details{
  If \code{ac} is a vector of Accession number,  pfamAC2PDB will
  return a list of associated PDB ID for the given \code{ac}.  If
  \code{ac} is NULL,  pfamAC2PDB(ac=NULL) (or pfamAC2PDB())
  will return a list of associated PDB ID for all the Accession number in
  the database.
  Extra information:  Start and end points
  More Details: PDB (\url{http://www.rcsb.org/pdb/index.html}), the single worldwide repository for the processing and distribution of 3-D biological macromolecular structure data.  Reference: H.M. Berman, J. Westbrook, Z. Feng, G. Gilliland, T.N. Bhat, H. Weissig, I.N. Shindyalov, P.E. Bourne:  The Protein Data Bank.   Nucleic Acids Research  ,  28  pp. 235-242 (2000)
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    AC2PDB <- pfamAC2PDB()
    head(AC2PDB)
    pfamAC2PDB(ac=sample(names(AC2PDB), 3))
}
}
\keyword{datasets}
