\name{@ANNOBJPREFIX@PATH2PROBE}
\alias{@ANNOBJPREFIX@PATH2PROBE}
\title{Map between Kyoto Encyclopedia of Genes and Genomes (KEGG)
  pathway identifiers and Manufacturer Identifiers}
\description{
 @ANNOBJPREFIX@PATH2PROBE is an R object that provides mappings between
 KEGG identifiers and manufacturer identifiers.
}
\details{
  Each KEGG identifier is mapped to a named vector of manufacturer identifiers. The name
  represents the KEGG identifier and the vector contains all manufacturer identifiers
  that are found in that particular pathway. An \code{NA} is reported
  for any KEGG identifier that cannot be mapped to any manufacturer identifiers.

  Pathway name for a given pathway identifier can be obtained using the KEGG
  data package that can either be built using AnnBuilder or downloaded
  from Bioconductor \url{http://www.bioconductor.org}.

  Graphic presentations of pathways are searchable at
  \url{http://www.genome.ad.jp/kegg/pathway.html} using pathway identifiers as keys.
  
  Mappings were based on data provided by: @PATH2PROBESOURCE@

}
\references{
\url{http://www.genome.ad.jp/kegg/}
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@PATH2PROBE)
    # Remove pathway identifiers that do not map to any probe id
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
        # The probe identifiers for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    } 
}

\keyword{datasets}

