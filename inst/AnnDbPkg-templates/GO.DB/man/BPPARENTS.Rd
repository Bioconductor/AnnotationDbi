\name{@ANNOBJPREFIX@BPPARENTS}
\alias{@ANNOBJPREFIX@BPPARENTS}
\title{Annotation of GO Identifiers to their Biological Process Parents} 
\description{
  This data set describes associations between GO molecular function (BP)
  terms and their direct parent BP terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  object mapping the GO BP terms to all direct parent terms, where a
  direct parent term is a more general GO term that immediately precedes
  the given GO term in the DAG.
}
\details{
   Each GO BP term is mapped to a named vector of GO BP terms. The name
  associated with the parent term will be either \emph{isa}, \emph{hasa} or \emph{partof},
  where \emph{isa} indicates that the child term is a more specific version
  of the parent, and \emph{hasa} and \emph{partof} indicate that the
  child term is a part of the parent. For example, a telomere is part of
  a chromosome.

  Biological process is defined as the broad biological goals, such as
  mitosis or purine metabolism, that are accomplished by ordered
  assemblies of molecular functions as defined by Gene Ontology
  Consortium.

  Mappings were based on data provided: @BPPARENTSSOURCE@

}
\references{
\url{http://www.geneontology.org/} and
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} 
}
\examples{
  # Convert the object to a list
  xx <- as.list(@ANNOBJPREFIX@BPPARENTS)
  # Remove GO IDs that do not have any parent
  xx <- xx[!is.na(xx)]
  if(length(xx) > 0){
     # Get the children GO IDs for the first elents of xx
        goids <- xx[[1]]
        # Find out the GO terms for the first parent goid
        GOID(@ANNOBJPREFIX@TERM[[goids[1]]])
        Term(@ANNOBJPREFIX@TERM[[goids[1]]])
        Synonym(@ANNOBJPREFIX@TERM[[goids[1]]])
        Secondary(@ANNOBJPREFIX@TERM[[goids[1]]])
        Definition(@ANNOBJPREFIX@TERM[[goids[1]]])
        Ontology(@ANNOBJPREFIX@TERM[[goids[1]]])
  }
}
\keyword{datasets}

