\name{@ANNOBJPREFIX@ENSEMBLPROT}
\alias{@ANNOBJPREFIX@ENSEMBLPROT}
\alias{@ANNOBJPREFIX@ENSEMBLPROT2ORF}
\title{Map Ensembl protein acession numbers with SGD Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@ENSEMBL is an R object that contains mappings between 
  SGD Gene identifiers and Ensembl protein accession numbers.    
}
\details{
  This object is a simple mapping of SGD Gene identifiers
  \url{ftp://genome-ftp.stanford.edu/pub/yeast/data_download} to Ensembl
  protein accession numbers.

  Mappings were based on data provided by:
  \url{ftp://ftp.ensembl.org/pub/current_fasta}
  \url{ftp://genome-ftp.stanford.edu/pub/yeast/data_download}
  \url{ftp://ftp.ncbi.nlm.nih.gov/gene/DATA}
  
}

\examples{
    x <- @ANNOBJPREFIX@ENSEMBLPROT
    # Get the SGD gene IDs that are mapped to an Ensembl ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Ensembl gene IDs for the first five proteins
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map ENSEMBLPROT2ORF:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ENSEMBLPROT2ORF)
    if(length(xx) > 0){
       # Gets the SGD gene IDs for the first five Ensembl IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



