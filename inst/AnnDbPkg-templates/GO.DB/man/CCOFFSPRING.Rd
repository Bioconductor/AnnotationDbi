\name{@ANNOBJPREFIX@CCOFFSPRING}
\alias{@ANNOBJPREFIX@CCOFFSPRING}
\title{Annotation of GO Identifiers to their Cellular Component Offspring} 
\description{
This is an R object mapping GO identifiers to all the GO
identifiers of their offspring based on the  cellular component (CC) GO terms. Offspring are
defined as the direct or indirect children/grand children nodes of a
given node for a GO identifier based on the directed acyclic graph defined by
Gene Ontology Consortium. GO terms for offspring nodes are more specific
than that of the parent.
}
\details{
  GO identifiers are keys and the corresponding offspring GO identifiers are
  values. Values are vectors of GO identifiers for all the offspring of the key
  GO identifier. GO identifiers that do not have any offspring node are assigned NA as
  the value. 

  Cellular component is defined as the subcellular structures,
  locations, and macromolecular complexes; examples include nucleus,
  telomere, and origin recognition complex as defined b y Gene Ontology
  Consortium. 
  
  Mappings were based on data provided: @CCOFFSPRINGSOURCE@

}
\references{
\url{http://www.geneontology.org/} and
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} 
}
\examples{
  # Convert the object to a list
  xx <- as.list(@ANNOBJPREFIX@CCOFFSPRING)
  # Remove GO identifiers that do not have any offspring
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
    # Get the offspring GO identifiers for the first two elents of xx
    goidentifiers <- xx[1:2]
  }
}
\keyword{datasets}

