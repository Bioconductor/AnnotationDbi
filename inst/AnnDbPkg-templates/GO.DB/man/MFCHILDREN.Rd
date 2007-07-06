\name{@ANNOBJPREFIX@MFCHILDREN}
\alias{@ANNOBJPREFIX@MFCHILDREN}
\title{Annotation of GO Identifiers to their Molecular Function Children} 
\description{
  This data set describes associations between GO molecular function (MF)
  terms and their direct children MF terms, based on the directed acyclic
  graph (DAG) defined by the Gene Ontology Consortium. The format is an R
  object mapping the GO MF terms to all direct children terms, where a
  direct child term is a more specific GO term that is immediately preceded
  by the given GO term in the DAG.
}
\details{
  Each GO MF term is mapped to a vector of children GO MF terms.

  Molecular function is defined as the tasks performed by individual
  gene products; examples are transcription factor and DNA helicase as
  defined by Gene Ontology Consortium.  
  
  Mappings were based on data provided by: @MFCHILDRENSOURCE@

}
\references{
\url{http://www.geneontology.org/} and
\url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} 
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@MFCHILDREN)
    # Remove GO identifiers that do not have any children
    xx <- xx[!is.na(xx)]
    if(length(xx) > 0){
        # Get the children GO identifiers for the first elents of xx
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

