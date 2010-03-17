\name{@ANNOBJPREFIX@ENSEMBLPROT}
\alias{@ANNOBJPREFIX@ENSEMBLPROT}
\alias{@ANNOBJPREFIX@ENSEMBLPROT2EG}
\title{Map Ensembl protein acession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@ENSEMBL is an R object that contains mappings between 
  Entrez Gene identifiers and Ensembl protein accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to Ensembl
  protein accession numbers.

  Mappings were based on data provided by:
  \url{ftp://ftp.ensembl.org/pub/current_fasta}
  \url{ftp://genome-ftp.stanford.edu/pub/yeast/data_download}
  \url{ftp://ftp.ncbi.nlm.nih.gov/gene/DATA}
}

\examples{
    x <- @ANNOBJPREFIX@ENSEMBLPROT
    # Get the entrez gene IDs that are mapped to an Ensembl ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Ensembl gene IDs for the first five proteins
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map ENSEMBLPROT2EG:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ENSEMBLPROT2EG)
    if(length(xx) > 0){
       # Gets the entrez gene IDs for the first five Ensembl IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



