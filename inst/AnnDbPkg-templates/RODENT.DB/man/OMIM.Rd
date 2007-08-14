\name{@ANNOBJPREFIX@OMIM}
\alias{@ANNOBJPREFIX@OMIM}
\title{Map between Entrez Gene Identifiers and Mendelian Inheritance in
  Man (MIM) identifiers}
\description{
  @ANNOBJPREFIX@OMIM is an R object that provides mappings between
  entrez gene identifiers and OMIM identifiers.
}
\details{
  Each entrez gene identifier is mapped to a vector of OMIM identifiers. The vector
  length may be one or longer, depending on how many OMIM identifiers the
  entrez gene identifier maps to. An \code{NA} is reported for any entrez gene
  identifier that cannot be mapped to an OMIM identifier at this time.
  
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
        mapped_genes <- mappedNames(x)
        # Convert to a list
        xx <- as.list(x, names=mapped_genes)
        if(length(xx) > 0) {
          # Get the OMIM for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}

\keyword{datasets}

