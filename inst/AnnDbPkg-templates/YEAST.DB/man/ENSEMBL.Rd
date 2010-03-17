\name{@ANNOBJPREFIX@ENSEMBL}
\alias{@ANNOBJPREFIX@ENSEMBL}
\alias{@ANNOBJPREFIX@ENSEMBL2ORF}
\title{Map Ensembl gene accession numbers with SGD Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@ENSEMBL is an R object that contains mappings between 
  SGD Gene identifiers and Ensembl gene accession numbers.    
}
\details{
  This object is a simple mapping of SGD Gene identifiers
  \url{ftp://genome-ftp.stanford.edu/pub/yeast/data_download} to Ensembl
  gene Accession Numbers.
  
  Mappings were based on data provided by ALL of these sources:
  \url{ftp://ftp.ensembl.org/pub/current_fasta}
  \url{ftp://genome-ftp.stanford.edu/pub/yeast/data_download}
  \url{ftp://ftp.ncbi.nlm.nih.gov/gene/DATA}

  This mapping is a combination of NCBI to ensembl IDs from BOTH NCBI
  and ensembl.  These mappings are based upon the ensembl
  table which is contains data from BOTH of these sources in an effort
  to maximize the chances that you will find a match.

  Mappings were based on data provided by: @ENSEMBLSOURCE@
}

\examples{
    x <- @ANNOBJPREFIX@ENSEMBL
    # Get the SGD gene IDs that are mapped to an Ensembl ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the Ensembl gene IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    #For the reverse map ENSEMBL2ORF:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ENSEMBL2ORF)
    if(length(xx) > 0){
       # Gets the SGD gene IDs for the first five Ensembl IDs
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}



