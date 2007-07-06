\name{@ANNOBJPREFIX@BPANCESTOR}
\alias{@ANNOBJPREFIX@BPANCESTOR}
\title{Annotation of GO Identifiers to their Biological Process Ancestors} 
\description{
  This data set describes associations between GO Biological Process (BP)
  terms and their ancestor BP terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  object mapping the GO BP terms to all ancestor terms, where an
  ancestor term is a more general GO term that precedes
  the given GO term in the DAG (in other words, the parents, and all
  their parents, etc.).
}
\details{
  Each GO BP term is mapped to a vector of ancestor GO BP terms.
  
  Biological process is defined as the broad biological goals, such as
  mitosis or purine metabolism, that are accomplished by ordered
  assemblies of molecular functions as defined by Gene Ontology
  Consortium.
    
  Mappings were based on data provided by: @BPANCESTORSOURCE@

}
\references{
\url{http://www.geneontology.org/} and
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} 
}
\examples{
  # Convert the object to a list
  xx <- as.list(@ANNOBJPREFIX@BPANCESTOR)
  # Remove GO IDs that do not have any ancestor
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
    # Get the ancestor GO IDs for the first two elents of xx
    goids <- xx[1:2]
  }
}
\keyword{datasets}

