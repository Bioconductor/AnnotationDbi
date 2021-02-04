\name{organismKEGGFrame}
\alias{organismKEGGFrame}
\alias{organismKEGGFrame}

\title{A data frame that maps species names to KEGG organisms}
\description{
  Create a data.frame that maps a species name (e.g. Homo sapeins) to the
  KEGG organsim notation (e.g. hsa).
}
\usage{
organismKEGGFrame()
}

\value{
  A data.frame with 2 columns, species and organsim.
}
\author{Kayla Interdonato}
\examples{
query <- organismKEGGFrame()
head(query)
}
