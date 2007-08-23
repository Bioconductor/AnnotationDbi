\name{@ANNOBJPREFIX@PATH}
\alias{@ANNOBJPREFIX@PATH}
\title{Mappings between Entrez Gene identifiers and KEGG pathway identifiers}
\description{
  KEGG (Kyoto Encyclopedia of Genes and Genomes) maintains pathway data
  for various organisms. @ANNOBJPREFIX@PATH maps entrez gene identifiers to the
  identifiers used by KEGG for pathways
}
\details{
  Each KEGG pathway has a name and identifier. Pathway name for a given pathway
  identifier can be obtained using the KEGG data package that can either be
  built using AnnBuilder or downloaded from Bioconductor
  \url{http://www.bioconductor.org}.

  Graphic presentations of pathways are searchable at
  url{http://www.genome.ad.jp/kegg/pathway.html} by using pathway identifiers as
  keys.   

  Mappings were based on data provided by: @PATHSOURCE@

}
\references{
  \url{http://www.genome.ad.jp/kegg/}
}
\examples{
        x <- @ANNOBJPREFIX@PATH
        # Get the entrez gene identifiers that are mapped to a KEGG pathway ID
        mapped_genes <- mappedKeys(x)
        # Convert to a list
        xx <- as.list(x, names=mapped_genes)
        if(length(xx) > 0) {
          # Get the PATH for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}



\name{@ANNOBJPREFIX@PATH2EG}
\alias{@ANNOBJPREFIX@PATH2EG}
\title{Map between Kyoto Encyclopedia of Genes and Genomes (KEGG)
  pathway identifiers and Entrez Gene Identifiers}
\description{
 @ANNOBJPREFIX@PATH2EG is an R object that provides mappings between
 KEGG identifiers and entrez gene identifiers.
}
\details{
  Each KEGG identifier is mapped to a named vector of entrez gene identifiers. The name
  represents the KEGG identifier and the vector contains all entrez gene identifiers
  that are found in that particular pathway. An \code{NA} is reported
  for any KEGG identifier that cannot be mapped to any entrez gene identifiers.

  Pathway name for a given pathway identifier can be obtained using the KEGG
  data package that can either be built using AnnBuilder or downloaded
  from Bioconductor \url{http://www.bioconductor.org}.

  Graphic presentations of pathways are searchable at
  \url{http://www.genome.ad.jp/kegg/pathway.html} using pathway identifiers as keys.

  Mappings were based on data provided by: @PATH2EGSOURCE@

}
\references{
\url{http://www.genome.ad.jp/kegg/}
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@PATH2EG)
    # Remove pathway identifiers that do not map to any entrez gene id
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
        # The entrez gene identifiers for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
    }
}

\keyword{datasets}
