\name{@ANNOBJPREFIX@CCANCESTOR}
\alias{@ANNOBJPREFIX@CCANCESTOR}
\title{Annotation of GO Identifiers to their Cellular Component Ancestors} 
\description{
  This data set describes associations between GO molecular function (CC)
  terms and their ancestor CC terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  object mapping the GO CC terms to all ancestor terms, where an
  ancestor term is a more general GO term that precedes
  the given GO term in the DAG (in other words, the parents, and all
  their parents, etc.).
}
\details{
  Each GO CC term is mapped to a vector of ancestor GO C terms.

  Cellular component is defined as the subcellular structures,
  locations, and macromolecular complexes; examples include nucleus,
  telomere, and origin recognition complex as defined by Gene Ontology
  Consortium.
    
  Mappings were based on data provided: @CCANCESTORSOURCE@

}
\references{
\url{http://www.geneontology.org/} and
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} 
}
\examples{
  # Convert the object to a list
  xx <- as.list(@ANNOBJPREFIX@CCANCESTOR)
  # Remove GO IDs that do not have any ancestor
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
    # Get the ancestor GO IDs for the first two elents of xx
    goids <- xx[1:2]
  }
}
\keyword{datasets}

