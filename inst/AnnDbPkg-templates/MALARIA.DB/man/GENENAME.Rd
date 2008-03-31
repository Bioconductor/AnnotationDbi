\name{@ANNOBJPREFIX@GENENAME}
\alias{@ANNOBJPREFIX@GENENAME}
\title{Map between ORF IDs and Genes}
\description{
  @ANNOBJPREFIX@GENENAME is an R object that maps ORF
  identifiers to the corresponding gene name.
}
\details{
  Each ORF identifier maps to a named vector containing the gene
  name. The vector name corresponds to the ORF identifier. If the 
  gene name is unknown, the vector will contain an \code{NA}.
  
  Gene names currently include both the official (validated by a
  nomenclature committee) and preferred names (interim selected for
  display) for genes. Efforts are being made to differentiate the two
  by adding a name to the vector.

  Mappings were based on data provided by: @GENENAMESOURCE@
  
}

\examples{
        x <- @ANNOBJPREFIX@GENENAME
        # Get the gene names that are mapped to an ORF identifier
        mapped_genes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_genes])
        if(length(xx) > 0) {
          # Get the GENE NAME for the first five genes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}


