\name{@ANNOBJPREFIX@PDB2AC}
\alias{@ANNOBJPREFIX@PDB2AC}
\title{Mappings from a PDB ID to a PFAM Accession number}
\description{
  This is an R object that always contains mappings from a PDB ID to a
  PFAM accession (AC)
}
\details{
  The PFAM accession number has been attached to the PDB IDs in this object.
  
  More Details: PDB (\url{http://www.rcsb.org/pdb/index.html}), the
  single worldwide repository for the processing and distribution of 3-D
  biological macromolecular structure data.  Reference: H.M. Berman,
  J. Westbrook, Z. Feng, G. Gilliland, T.N. Bhat, H. Weissig,
  I.N. Shindyalov, P.E. Bourne:  The Protein Data Bank.   Nucleic Acids
  Research  ,  28  pp. 235-242 (2000)
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\examples{

        #To map from PDB to PFAM IDs:
        x <- @ANNOBJPREFIX@PDB2AC
        # Get the PDB identifiers that are mapped to a PFAM ID
        mapped_keys <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_keys])
        if(length(xx) > 0) {
          # Get the PDB ID for the first five CAZYs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }

}
\keyword{datasets}
