\name{@ANNOBJPREFIX@PATH2TAIR}
\alias{@ANNOBJPREFIX@PATH2TAIR}
\title{Map between Kyoto Encyclopedia of Genes and Genomes (KEGG)
  pathway identifiers and TAIR Identifiers}
\description{
 @ANNOBJPREFIX@PATH2TAIR is an R object that provides mappings between
 KEGG identifiers and TAIR identifiers.
}
\details{
  Each KEGG identifier is mapped to a named vector of manufacturer
  identifiers. The name represents the KEGG identifier and the vector
  contains all TAIR identifiers that are found in that
  particular pathway. An \code{NA} is reported for any KEGG identifier
  that cannot be mapped to any TAIR identifiers.

  Pathway name for a given pathway identifier can be obtained using the KEGG
  data package that can either be built using AnnBuilder or downloaded
  from Bioconductor \url{http://www.bioconductor.org}.

  Graphic presentations of pathways are searchable at
  \url{http://www.genome.ad.jp/kegg/pathway.html} using pathway
  identifiers as keys.
  
  Mappings were based on data provided by: @PATH2TAIRSOURCE@

}
\references{
\url{http://www.genome.ad.jp/kegg/}
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@PATH2TAIR)
    # Remove pathway identifiers that do not map to any tair id
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
        # The tair identifiers for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    } 
}

\keyword{datasets}

