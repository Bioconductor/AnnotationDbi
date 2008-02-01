\name{@ANNOBJPREFIX@SCOP}
\alias{@ANNOBJPREFIX@SCOP}
\title{Mappings from a PFAM Accession number to a SCOP ID}
\description{
  This is an R object that always contains mappings from a PFAM
  accession (AC) to a SCOP ID
}
\details{
  The SCOP ID along with the start point and end point have been attached
  to the PFAM accessions in this object.

  More Details: Structural Classification of Proteins
  (\url{http://scop.mrc-lmb.cam.ac.uk/scop/index.html}).  Reference:
  Murzin A. G., Brenner S. E., Hubbard T., Chothia C. (1995). SCOP: a
  structural classification of proteins database for the investigation
  of sequences and structures. J. Mol. Biol. 247, 536-540  
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\examples{

        #To map from PFAM to SCOP:
        x <- @ANNOBJPREFIX@SCOP
        # Get the PFAM identifiers that are mapped to a SCOP
        mapped_keys <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_keys])
        if(length(xx) > 0) {
          # Get the SCOP info for the first five PFAM IDs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }

}
\keyword{datasets}
