\name{@ANNOBJPREFIX@PATH}
\alias{@ANNOBJPREFIX@PATH}
\alias{@ANNOBJPREFIX@PATH2ORF}
\title{Mappings between ORF identifiers and KEGG pathway identifiers}
\description{
  KEGG (Kyoto Encyclopedia of Genes and Genomes) maintains pathway data
  for various organisms. @ANNOBJPREFIX@PATH maps ORF identifiers to the
  identifiers used by KEGG for pathways.  The reverse map
  @ANNOBJPREFIX@PATH2ORF maps back from KEGG pathway IDs to the ORF
  identifiers.
}
\details{
  Each KEGG pathway has a name and identifier. Pathway name for a given pathway
  identifier can be obtained using the KEGG data package that can either be
  built using AnnBuilder or downloaded from Bioconductor
  \url{http://www.bioconductor.org}.

  Graphic presentations of pathways are searchable at
  url http://www.genome.ad.jp/kegg/pathway.html by using pathway identifiers as
  keys.   

  Mappings were based on data provided by: @PATHSOURCE@

}
\references{
  \url{http://www.genome.ad.jp/kegg/}
}
\examples{
    x <- @ANNOBJPREFIX@PATH
    # Get the ORF identifiers that are mapped to a KEGG pathway ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the PATH for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@PATH2ORF)
    # Remove pathway identifiers that do not map to any ORF id
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
        # The ORF identifiers for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }
}
\keyword{datasets}


