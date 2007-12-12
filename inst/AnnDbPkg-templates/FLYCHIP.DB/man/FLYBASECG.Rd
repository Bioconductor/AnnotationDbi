\name{@ANNOBJPREFIX@FLYBASECG}
\alias{@ANNOBJPREFIX@FLYBASECG}
\alias{@ANNOBJPREFIX@FLYBASECG2PROBE}
\title{Map FlyBase CG accession numbers with manufacturer identifiers}  
\description{ 
  @ANNOBJPREFIX@FLYBASE is an R object that contains mappings between 
  manufacturer identifiers and FlyBase CG accession numbers.  These
  accessions are used by Ensembl and supported by FlyBase.
}
\details{
  This object is a simple mapping of manufacturer identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Flybase
  CG Accession Numbers.

  Mappings were based on data provided by: @FLYBASESOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@FLYBASECG
    # Get the manufacturer IDs that are mapped to a Flybase CG ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the FlyBase CG IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map FLYBASECG2EG:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@FLYBASECG2PROBE)
    if(length(xx) > 0){
       # Gets the manufacturer IDs for the first five FlyBase CG IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



