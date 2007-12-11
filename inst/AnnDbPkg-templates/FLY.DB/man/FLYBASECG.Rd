\name{@ANNOBJPREFIX@FLYBASECG}
\alias{@ANNOBJPREFIX@FLYBASECG}
\alias{@ANNOBJPREFIX@FLYBASECG2EG}
\title{Map FlyBase CG Acession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@FLYBASE is an R object that contains mappings between 
  Entrez Gene identifiers and FlyBase CG accession numbers.  These
  accessions are used by ensembl and supported by flybase.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Flybase
  CG accession numbers.

  Mappings were based on data provided by: @FLYBASESOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@FLYBASECG
    # Get the entrez gene IDs that are mapped to a Flybase CG ID
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
    xx <- as.list(@ANNOBJPREFIX@FLYBASECG2EG)
    if(length(xx) > 0){
       # Gets the entrez gene IDs for the first five FlyBase CG IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



