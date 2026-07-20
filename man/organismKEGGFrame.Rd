\name{organismKEGGFrame}
\alias{organismKEGGFrame}
\alias{genomeKEGGFrame}

\title{A data frame that maps species names to KEGG organisms}
\description{
  Create a data.frame that maps a species name (e.g. Homo sapeins) to the
  KEGG organsim notation (e.g. hsa).
}
\usage{
organismKEGGFrame()

genomeKEGGFrame()
}

\value{
  A data.frame with 2 columns, species and organsim.
}
\author{Kayla Interdonato}
\examples{
## DEPRECATED
query <- organismKEGGFrame()
head(query)

query <- genomeKEGGFrame()
head(query)

}
