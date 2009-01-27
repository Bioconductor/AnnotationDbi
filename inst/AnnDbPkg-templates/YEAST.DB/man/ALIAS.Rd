\name{@ANNOBJPREFIX@ALIAS}
\alias{@ANNOBJPREFIX@ALIAS}
\alias{@ANNOBJPREFIX@ALIAS2ORF}
\title{Map Open Reading Frame (ORF) Identifiers to Alias Gene Names}
\description{
  A set of gene names may have been used to report yeast genes
  represented by ORF identifiers. One of these names is chosen to be the primary
  gene name, and the others are considered aliases. This R object
  provides mappings between the primary name and aliases.
}
\details{
  Each primary name maps to a vector of alias names. If there are no
  aliases, the vector will contain \code{NA}.
  
  Annotation based on data provided by: @ALIASSOURCE@

}
\references{
\url{http://www.yeastgenome.org/DownloadContents.shtml}
}
\examples{
    x <- @ANNOBJPREFIX@ALIAS
    # Get the probe identifiers that are mapped to alias names
    mapped_probes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_probes])
    if(length(xx) > 0) {
        # Get the alias names for the first five probes
        xx[1:5]
        # For the first probe
        xx[[1]]
    }
}
\keyword{datasets}

