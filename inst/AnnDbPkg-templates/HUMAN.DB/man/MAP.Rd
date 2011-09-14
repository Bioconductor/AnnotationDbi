\name{@ANNOBJPREFIX@MAP}
\alias{@ANNOBJPREFIX@MAP}
\alias{@ANNOBJPREFIX@MAP2EG}
\title{Map between Entrez Gene Identifiers and cytogenetic maps/bands}
\description{
   @ANNOBJPREFIX@MAP is an R object that provides mappings between
   entrez gene identifiers and cytoband locations.
}
\details{
  Each entrez gene identifier is mapped to a vector of cytoband locations. The
  vector length may be one or longer, if there are multiple reported
  chromosomal locations for a given gene. An \code{NA} is reported for
  any entrez gene identifiers that cannot be mapped to a cytoband at this time.
  
  Cytogenetic bands for most higher organisms are labeled p1, p2, p3,
  q1, q2, q3 (p and q are the p and q arms), etc., counting from the
  centromere out toward the telomeres. At higher resolutions, sub-bands
  can be seen within the bands. The sub-bands are also numbered from the
  centromere out toward the telomere. Thus, a label of 7q31.2 indicates
  that the band is on chromosome 7, q arm, band 3, sub-band 1, and
  sub-sub-band 2.   

  Mappings were based on data provided by: @MAPSOURCE@

}
\references{
  \url{http://www.ncbi.nlm.nih.gov}
}
\examples{
        x <- @ANNOBJPREFIX@MAP
        # Get the entrez gene identifiers that are mapped to any cytoband
        mapped_genes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_genes])
        if(length(xx) > 0) {
          # Get the ids for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
        #For the reverse map @ANNOBJPREFIX@MAP2EG
        x <- @ANNOBJPREFIX@MAP2EG
        # Get the cytobands that are mapped to any entrez gene id
        mapped_bands <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_bands])
        if(length(xx) > 0) {
          # Get the bands for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}


