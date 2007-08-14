\name{@ANNOBJPREFIX@UNIGENE}
\alias{@ANNOBJPREFIX@UNIGENE}
\title{Map between Entrez Gene Identifiers and UniGene cluster identifiers}
\description{
   @ANNOBJPREFIX@UNIGENE is an R object that provides mappings between
   entrez gene identifiers and UniGene identifiers.
}
\details{
  Each entrez gene identifier is mapped to a UniGene identifier. An \code{NA} is
  reported if the entrez gene identifier cannot be mapped to UniGene at this time. 

  A UniGene identifier represents a cluster of sequences of a gene. Using
  UniGene identifiers one can query the UniGene database for information about
  the sequences.  
  
  Mappings were based on data provided by: @UNIGENESOURCE@

}
\references{
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene}
}
\examples{
        x <- @ANNOBJPREFIX@UNIGENE
        # Get the entrez gene identifiers that are mapped to an UNIGENE ID
        mapped_genes <- mappedNames(x)
        # Convert to a list
        xx <- as.list(x, names=mapped_genes)
        if(length(xx) > 0) {
          # Get the UNIGENE for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}

\keyword{datasets}

