\name{@ANNOBJPREFIX@MFANCESTOR}
\alias{@ANNOBJPREFIX@MFANCESTOR}
\title{Annotation of GO identifiers to their Molecular Function Ancestors} 
\description{
  This data set describes associations between GO molecular function (MF)
  terms and their ancestor MF terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  object mapping the GO MF terms to all ancestor terms, where an
  ancestor term is a more general GO term that precedes
  the given GO term in the DAG (in other words, the parents, and all
  their parents, etc.).
}
\details{
  Each GO MF term is mapped to a vector of ancestor GO MF terms.

  Molecular function is defined as the tasks performed by individual
  gene products; examples are transcription factor and DNA helicase as
  defined by Gene Ontology Consortium.  
  
  Mappings were based on data provided: @MFANCESTORSOURCE@

}
\references{
\url{http://www.geneontology.org/} and
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} 
}
\examples{
  # Convert the object to a list
  xx <- as.list(@ANNOBJPREFIX@MFANCESTOR)
  # Remove GO identifiers that do not have any ancestor
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
    # Get the ancestor GO identifiers for the first two elents of xx
    goids <- xx[1:2]
  }
}
\keyword{datasets}

