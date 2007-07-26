\name{@ANNOBJPREFIX@CHRLENGTHS}
\alias{@ANNOBJPREFIX@CHRLENGTHS}
\title{A named vector for the length of each of the chromosomes}
\description{
  @ANNOBJPREFIX@CHRLENGTHS provides the length measured in base pairs for
  each of the chromosomes. 
}
\details{
   This is a named vector with chromosome numbers as the names and the
   corresponding lengths for chromosomes as the values.

   Total lengths of chromosomes were derived by calculating the
   number of base pairs on the sequence string for each chromosome.

}
\examples{
    tt <- @ANNOBJPREFIX@CHRLENGTHS
    # Length of chromosome 1
    tt["1"]
}
\keyword{datasets}

