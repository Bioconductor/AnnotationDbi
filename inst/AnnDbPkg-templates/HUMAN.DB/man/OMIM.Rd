\name{@ANNOBJPREFIX@OMIM}
\alias{@ANNOBJPREFIX@OMIM}
\alias{@ANNOBJPREFIX@OMIM2EG}
\title{Map between Entrez Gene Identifiers and Mendelian Inheritance in
  Man (MIM) identifiers}
\description{
  @ANNOBJPREFIX@OMIM is an R object that provides mappings between
  entrez gene identifiers and OMIM identifiers.
}
\details{
  Each entrez gene identifier is mapped to an OMIM identifier. An
  \code{NA} is reported for any entrez gene identifier that cannot be
  mapped to an OMIM identifier at this time.
  
  OMIM is based upon the book Mendelian Inheritance in Man
  (V. A. McKusick) and focuses primarily on inherited or heritable
  genetic diseases. It contains textual information, pictures, and reference
  information that can be searched using various terms, among which the
  MIM number is one. 
  
  Mappings were based on data provided by: @OMIMSOURCE@

}
\references{
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene}
  \url{http://www3.ncbi.nlm.nih.gov/entrez/query.fcgi?db=OMIM}
}
\examples{
        x <- @ANNOBJPREFIX@OMIM
        # Get the entrez gene identifiers that are mapped to a OMIM ID
        mapped_genes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_genes])
        if(length(xx) > 0) {
          # Get the OMIM for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }

        # For the reverse map:
        x <- @ANNOBJPREFIX@OMIM2EG
        # Get the OMIM IDs that are mapped to the entrez gene IDs
        mapped_OMIMs <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_OMIMs])
        if(length(xx) > 0) {
          # Get the entrez gene ID for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}

\keyword{datasets}
