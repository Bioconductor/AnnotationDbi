\name{@ANNOBJPREFIX@REFSEQ}
\alias{@ANNOBJPREFIX@REFSEQ}
\title{Map between Manufacturer Identifiers and RefSeq Identifiers}
\description{
   @ANNOBJPREFIX@REFSEQ is an R object that provides mappings between
   manufacturer identifiers and RefSeq identifiers.
}
\details{
  Each manufacturer identifier is mapped to a named vector of RefSeq identifiers. The
  name represents the manufacturer identifier and the vector contains all RefSeq
  identifiers that can be mapped to that manufacturer identifier. The length of the
  vector may be one or greater, depending on how many RefSeq identifiers a given
  manufacturer identifier can be mapped to. An \code{NA} is reported for any
  manufacturer identifier that cannot be mapped to a RefSeq identifier at this time.

  RefSeq identifiers differ in format according to the type of record the identifiers
  are for as shown below:

  NG\_XXXXX: RefSeq accessions for genomic region (nucleotide) records

  NM\_XXXXX: RefSeq accessions for mRNA records

  NC\_XXXXX: RefSeq accessions for chromosome records

  NP\_XXXXX: RefSeq accessions for protein records

  XR\_XXXXX: RefSeq accessions for model RNAs that are not associated with
  protein products

  XM\_XXXXX: RefSeq accessions for model mRNA records

  XP\_XXXXX: RefSeq accessions for model protein records

  Where XXXXX is a sequence of integers.

  NCBI \url{http://www.ncbi.nlm.nih.gov/RefSeq/} allows users to query
  the RefSeq database using RefSeq identifiers.

  Mappings were based on data provided by: @REFSEQSOURCE@

}
\references{
    \url{http://www.ncbi.nlm.nih.gov}
    \url{http://www.ncbi.nlm.nih.gov/RefSeq/} 
}
\examples{
        x <- @ANNOBJPREFIX@REFSEQ
        # Get the probe identifiers that are mapped to any RefSeq ID
        mapped_probes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_probes])
        if(length(xx) > 0) {
          # Get the REFSEQ for the first five probes
          xx[1:5]
          # Get the first one
          xx[[1]]
        }
}

\keyword{datasets}

