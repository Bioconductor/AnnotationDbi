\name{pfamPDB2AC}
\alias{pfamPDB2AC}
\title{Mappings from PDB ID to Accession number}
\description{
  Mapping function from PDB ID (PDB) to the associated Accession number (AC)
}
\usage{
pfamPDB2AC(pdb=NULL)
}
\arguments{
  \item{pdb}{A character vector of PDB ID or NULL (default)}
}
\value{
  A list of Accession number.
}
\details{
  If \code{pdb} is a vector of PDB ID,  pfamPDB2AC will
  return a list of associated Accession number for the given \code{pdb}.  If
  \code{pdb} is NULL,  pfamPDB2AC(pdb=NULL) (or pfamPDB2AC())
  will return a list of associated Accession number for all the PDB ID in
  the database.
  Extra information:  None
  More Details: PDB (\url{http://www.rcsb.org/pdb/index.html}), the single worldwide repository for the processing and distribution of 3-D biological macromolecular structure data.  Reference: H.M. Berman, J. Westbrook, Z. Feng, G. Gilliland, T.N. Bhat, H. Weissig, I.N. Shindyalov, P.E. Bourne:  The Protein Data Bank.   Nucleic Acids Research  ,  28  pp. 235-242 (2000)
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\author{Ting-Yuan Liu}
\examples{
\dontrun{
    PDB2AC <- pfamPDB2AC()
    head(PDB2AC)
    pfamPDB2AC(pdb=sample(names(PDB2AC), 3))
}
}
\keyword{datasets}
