\name{@ANNOBJPREFIX@OBSOLETE}
\alias{@ANNOBJPREFIX@OBSOLETE}
\title{Annotation of GO identifiers by terms defined by Gene Ontology
  Consortium and their status are obsolete}
\description{
This is an R object mapping GO identifiers to the specific
terms in defined by Gene Ontology Consortium and their definition are obsolete
}
\details{
  All the obsolete GO terms that are collected in this index will no longer exist 
  in other mapping objects.

  Mappings were based on data provided by: @OBSOLETESOURCE@

}
\references{
  \url{http://www.ncbi.nlm.nih.gov/LocusLink} 
}
\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@TERM)
    if(length(xx) > 0){
        # Get the TERMS for the first elent of xx
        GOID(xx[[1]])
        Ontology(xx[[1]])
    }
}
\keyword{datasets}

