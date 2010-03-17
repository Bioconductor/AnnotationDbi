\name{@ANNOBJPREFIX@UCSCKG}
\alias{@ANNOBJPREFIX@UCSCKG}
\title{This mapping has been deprecated and will no longer be available
  after bioconductor 2.6.  See the details section for how you can live
  without it.  For now, it is a map of UCSC "Known Gene" accession numbers with Entrez Gene identifiers}  
\description{ 
  @ANNOBJPREFIX@UCSCKG is an R object that contains mappings between 
  Entrez Gene identifiers and UCSC "Known Gene" accession numbers.    
}
\details{
  This object is a simple mapping of Entrez Gene identifiers
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene} to UCSC
  "Known Gene" Accession Numbers.  Please be advised that UCSC "Known
  Genes" are not more like putative transcripts transcript identifiers
  than they are gene identifiers.  

  Mappings were based on data provided by: @CHRLOCSOURCE@
  
  This mapping is based on the very latest build available at UCSC for
  this organism as of March 2010.  2.6 is the last release where you
  can expect it to be here.  The GenomicFeatures package contains
  functionality that replaces the need for this mapping.  Please see the
  GenomicFeatures vignette for details on how to use it.
  
}

\examples{
    x <- @ANNOBJPREFIX@UCSCKG
    # Get the entrez gene IDs that are mapped to a UCSC KG ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the UCSC gene IDs for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
}
\keyword{datasets}



