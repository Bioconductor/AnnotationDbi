\name{@ANNOBJPREFIX@MFOFFSPRING}
\alias{@ANNOBJPREFIX@MFOFFSPRING}
\title{Annotation of GO Identifiers to their Molecular Function Offspring} 
\description{
  This data set describes associations between GO molecular function (MF)
  terms and their offspring MF terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  object mapping the GO MF terms to all offspring terms, where an
  ancestor term is a more specific GO term that is preceded
  by the given GO term in the DAG (in other words, the children and all
  their children, etc.).
}
\details{
  Each GO MF term is mapped to a vector of offspring GO MF terms.

  Molecular function is defined as the tasks performed by individual
  gene products; examples are transcription factor and DNA helicase as
  defined by Gene Ontology Consortium.  
  
  Mappings were based on data provided by: @MFOFFSPRINGSOURCE@

}
\references{
\url{http://www.geneontology.org/} and
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} 
}
\examples{
  # Convert the object to a list
  xx <- as.list(@ANNOBJPREFIX@MFOFFSPRING)
  # Remove GO identifiers that do not have any offspring
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
    # Get the offspring GO identifiers for the first two elents of xx
  goids <- xx[1:2]
  }
}
\keyword{datasets}

