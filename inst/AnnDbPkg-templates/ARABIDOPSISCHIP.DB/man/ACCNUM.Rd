\name{@ANNOBJPREFIX@ACCNUM}

\alias{@ANNOBJPREFIX@ACCNUM}
% \alias{@ANNOBJPREFIX@MULTIHIT}

\title{Map between manufacturer IDs and AGI locus IDs}

\description{ 
  \code{@ANNOBJPREFIX@ACCNUM} is an R object that provide mappings
  between manufacturer IDs and AGI locus IDs.
}

\details{
  Each manufacturer ID is mapped to a vector of AGI locus IDs.

  If a manufacturer ID is mapped to multiple AGI locus IDs, then all of
  these AGI locus IDs will be listed.

  For \code{@ANNOBJPREFIX@ACCNUM} an \code{NA} is assigned to those
  manufacturer IDs that can not be mapped to an AGI locus ID at this time.

  Mappings were based on data provided by: @ACCNUMSOURCE@
}

\seealso{
  \code{\link[AnnotationDbi:Bimap-toTable]{nhit}}
}

\examples{
  x <- @ANNOBJPREFIX@ACCNUM
  # Get the probe identifiers that are mapped to a chromosome
  mapped_probes <- mappedkeys(x)
  # Convert to a list
  xx <- as.list(x[mapped_probes])
  if(length(xx) > 0) {
    # Get the AGI locus IDs for the first five probes
    xx[1:5]
    # Get the first one
    xx[[1]]
  }

##   y <- @ANNOBJPREFIX@MULTIHIT
##  identical(keys(x), keys(y))    # TRUE

#  nhx <- nhit(x)
#  nhy <- nhit(y)
#  identical(names(nhx), keys(x)) # TRUE
#  identical(names(nhy), keys(y)) # TRUE
#  table(nhx)
#  table(nhy)
#  onehit_probes <- names(nhx)[nhx != 0 & nhy == 0]
#  x[[onehit_probes[1]]]          # a single AGI locus ID
#  y[[onehit_probes[1]]]          # NA
#  multihit_probes <- names(nhx)[nhy != 0]
#  x[[multihit_probes[1]]]        # "multiple"
#  y[[multihit_probes[1]]]        # several AGI locus IDs
#  nohit_probes <- names(nhx)[nhx == 0]
#  x[[nohit_probes[1]]]           # NA
#  y[[nohit_probes[1]]]           # NA
#  any(nhx == 0 & nhy != 0)       # FALSE
#  ## Back to a more "normal" map (that combines the data from 'x' and 'y')
#  xy <- as(x, "AnnDbBimap")      # 'as(y, "AnnDbBimap")' works too
#  xy[[onehit_probes[1]]]         # a single AGI locus ID
#  xy[[multihit_probes[1]]]       # several AGI locus IDs
#  xy[[nohit_probes[1]]]          # NA

}

\keyword{datasets}

