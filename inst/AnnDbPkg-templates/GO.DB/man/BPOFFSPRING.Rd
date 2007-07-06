\name{@ANNOBJPREFIX@BPOFFSPRING}
\alias{@ANNOBJPREFIX@BPOFFSPRING}
\title{Annotation of GO Identifiers to their Biological Process Offspring} 
\description{
  This data set describes associations between GO molecular function (BP)
  terms and their offspring BP terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  object mapping the GO BP terms to all offspring terms, where an
  ancestor term is a more specific GO term that is preceded
  by the given GO term in the DAG (in other words, the children and all
  their children, etc.).
}
\details{
  Each GO BP term is mapped to a vector of offspring GO BP terms.

  Biological process is defined as the broad biological goals, such as
  mitosis or purine metabolism, that are accomplished by ordered
  assemblies of molecular functions as defined by Gene Ontology
  Consortium.
  
  Mappings were based on data provided by: @BPOFFSPRINGSOURCE@

}
\references{
\url{http://www.geneontology.org/} and
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} 
}
\examples{
  # Convert the object to a list
  xx <- as.list(@ANNOBJPREFIX@BPOFFSPRING)
  # Remove GO IDs that do not have any offspring
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
    # Get the offspring GO IDs for the first two elents of xx
    goids <- xx[1:2]
  }
}
\keyword{datasets}

