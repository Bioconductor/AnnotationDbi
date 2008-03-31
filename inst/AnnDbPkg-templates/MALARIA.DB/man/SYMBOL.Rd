\name{@ANNOBJPREFIX@SYMBOL}
\alias{@ANNOBJPREFIX@SYMBOL}
\alias{@ANNOBJPREFIX@SYMBOL2ORF}
\title{Map between ORF Identifiers and Gene Symbols}
\description{
  @ANNOBJPREFIX@SYMBOL is an R object that provides mappings between
  ORF identifiers and gene abbreviations. 
}
\details{
  Each ORF identifier is mapped to the a common abbreviation for 
  the corresponding gene. An \code{NA} is reported if there is no known
  abbreviation for a given gene.
  
  Symbols typically consist of 3 letters that define either a single
  gene (ABC) or multiple genes (ABC1, ABC2, ABC3). Gene symbols can be
  used as key words to query public databases such as ORF.

  Mappings were based on data provided by: @SYMBOLSOURCE@
  
}
\references{
  \url{http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene}
}
\examples{
        x <- @ANNOBJPREFIX@SYMBOL
        # Get the gene symbol that are mapped to an ORF identifiers
        mapped_genes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_genes])
        if(length(xx) > 0) {
          # Get the SYMBOL for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
        # For the reverse map:
        x <- @ANNOBJPREFIX@SYMBOL2ORF
        # Get the ORF identifiers that are mapped to a gene symbol
        mapped_genes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_genes])
        if(length(xx) > 0) {
          # Get the ORF ID for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}

\keyword{datasets}


