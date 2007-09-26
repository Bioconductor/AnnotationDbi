\name{@ANNOBJPREFIX@DESCRIPTION}
\alias{@ANNOBJPREFIX@DESCRIPTION}
\title{An annotation data file that maps Open Reading Frame (ORF)
  identifiers to textural descriptions of the corresponding genes}
\description{
  @ANNOBJPREFIX@DESCRIPTION maps yeast ORF identifiers to descriptive information
  about genes corresponding to the ORF identifiers 
}
\details{
  This is an R object containing key and value pairs. Keys are
  ORF identifiers and values are the corresponding descriptions of genes. Values
  are vectors of length 1. Probe identifiers that can not be mapped to
  descriptive information are assigned a value NA.
  
  Annotation based on data provided by: @DESCRIPTIONSOURCE@

}
\references{
\url{http://www.yeastgenome.org/DownloadContents.shtml}
}
\examples{
        x <- @ANNOBJPREFIX@DESCRIPTION
        # Get the probe identifiers that are mapped to gene descriptions
        mapped_probes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_probes])
        if(length(xx) > 0) {
          # Get the gene descriptions for the first five probes
          xx[1:5]
          # For the first probe
          xx[[1]]
        }
}
\keyword{datasets}

