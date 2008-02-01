\name{@ANNOBJPREFIX@SCOP2AC}
\alias{@ANNOBJPREFIX@SCOP2AC}
\title{Mappings from a SCOP ID to a PFAM Accession number}
\description{
  This is an R object that always contains mappings from a SCOP ID to a
  PFAM accession (AC)
}
\details{
  The PFAM accession number has been attached to the SCOP IDs in this object.

  More Details: Structural Classification of Proteins
  (\url{http://scop.mrc-lmb.cam.ac.uk/scop/index.html}).  Reference:
  Murzin A. G., Brenner S. E., Hubbard T., Chothia C. (1995). SCOP: a
  structural classification of proteins database for the investigation
  of sequences and structures. J. Mol. Biol. 247, 536-540  
}
\references{\url{http://www.sanger.ac.uk/Software/Pfam/} and
  \url{ftp://ftp.sanger.ac.uk/pub/databases/Pfam/current_release/userman.txt}}
\examples{

        #To map from SCOP to PFAM IDs:
        x <- @ANNOBJPREFIX@SCOP2AC
        # Get the SCOP identifiers that are mapped to a PFAM ID
        mapped_keys <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_keys])
        if(length(xx) > 0) {
          # Get the SCOP ID for the first five CAZYs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }

}
\keyword{datasets}
