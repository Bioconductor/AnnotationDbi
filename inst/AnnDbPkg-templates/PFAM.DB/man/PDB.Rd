\name{@ANNOBJPREFIX@PDB}
\alias{@ANNOBJPREFIX@PDB}
\title{Mappings from a PFAM Accession number to a PDB ID}
\description{
  This is an R object that always contains mappings from a PFAM
  accession (AC) to a PDB ID
}
\details{
  The PDB ID along with the start point and end point have been attached
  to the PFAM accessions in this object.

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

        #To map from PFAM to PDB:
        x <- @ANNOBJPREFIX@PDB
        # Get the PFAM identifiers that are mapped to a PDB
        mapped_keys <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_keys])
        if(length(xx) > 0) {
          # Get the PDB info for the first five PFAM IDs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }

}
\keyword{datasets}
