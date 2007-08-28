\name{@ANNOBJPREFIX@ACCNUM}
\alias{@ANNOBJPREFIX@ACCNUM}
\title{Map Entrez Gene identifiers to GenBank Accession Numbers}  
\description{ 
  @ANNOBJPREFIX@ACCNUM is an R object that contains mappings between 
  Entrez Gene identifiers and GenBank accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to all possible GenBank 
  accession numbers.

  Mappings were based on data provided by: @ACCNUMSOURCE@
}

\examples{
        x <- @ANNOBJPREFIX@ACCNUM
        # Get the entrez gene identifiers that are mapped to an ACCNUM
        mapped_genes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x, keys=mapped_genes)
        if(length(xx) > 0) {
          # Get the ACCNUM for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}




\name{@ANNOBJPREFIX@ACCNUM2EG}
\alias{@ANNOBJPREFIX@ACCNUM2EG}
\title{Map between GenBank Accession Numbers and Entrez Gene Identifiers}
\description{
  @ANNOBJPREFIX@ENZYME2EG is an R object that maps GenBank Accession
  Numbers to entrez gene identifiers.
}
\details{
  This object is a simple mapping of GenBank Accession numbers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Entrez gene Identifiers.

  Mappings were based on data provided by: @ACCNUMSOURCE@
}
\examples{
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ACCNUM2EG)
   if(length(xx) > 0){
       # Gets the entrez gene identifiers for the first five enzyme
       #commission numbers
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}
