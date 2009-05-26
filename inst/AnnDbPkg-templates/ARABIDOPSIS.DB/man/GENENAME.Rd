\name{@ANNOBJPREFIX@GENENAME}
\alias{@ANNOBJPREFIX@GENENAME}
\title{Map between TAIR IDs and Genes}
\description{
  @ANNOBJPREFIX@GENENAME is an R object that maps TAIR
  identifiers to the corresponding gene name.
}
\details{
  Each TAIR identifier maps to a named vector containing the gene
  name. The vector name corresponds to the TAIR identifier. If
  the gene name is unknown, the vector will contain an \code{NA}.
  
  Gene names currently include both the official (validated by a
  nomenclature committee) and preferred names (interim selected for
  display) for genes. Efforts are being made to differentiate the two
  by adding a name to the vector.

  Mappings were based on data provided by: @GENENAMESOURCE@
  
}

\examples{
        x <- @ANNOBJPREFIX@GENENAME
        # Get the TAIR identifiers that are mapped to a gene name
        mapped_tairs <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_tairs])
        if(length(xx) > 0) {
          # Get the GENENAME for the first five tairs
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}
\keyword{datasets}

