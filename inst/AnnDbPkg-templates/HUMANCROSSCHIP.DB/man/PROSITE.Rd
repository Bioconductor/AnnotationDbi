\name{@ANNOBJPREFIX@PROSITE}
\alias{@ANNOBJPREFIX@PROSITE}
\title{Map Manufacturer IDs to PROSITE ID}
\description{
  @ANNOBJPREFIX@PROSITE is an R object that provides mappings between a
 manufacturer identifier and the associated PROSITE identifiers.
}
\details{
  Each manufacturer identifier maps to a named vector of PROSITE identifiers.  The name
  for each PROSITE identifier is the IPI accession numbe where this PROSITE identifier is found.

  If the PROSITE is a named NA, it means that the associated Entrez Gene id
  of this manufacturer identifier is found in an IPI entry of the IPI database,
  but there is no PROSITE identifier in the entry.

  If the PROSITE is a non-named NA, it means that the associated Entrez Gene id
  of this manufacturer identifier is not found in any IPI entry of the IPI database.

   Mappings were based on data provided by: @PROSITESOURCE@

}
\references{
  Kersey P. J., Duarte J., Williams A., Karavidopoulou Y., Birney E., Apweiler R.
  The International Protein Index: An integrated database for proteomics experiments.
  Proteomics 4(7): 1985-1988 (2004)
  \url{http://www.ebi.ac.uk/IPI/IPIhelp.html}
}
\examples{
	        x <- @ANNOBJPREFIX@PROSITE
        # Get the probe identifiers that are mapped to any PROSITE ID
        mapped_probes <- mappedkeys(x)
        # Convert to a list
        xxx <- as.list(x[mapped_probes])
        # randomly display 10 probes
        xxx[sample(1:length(xxx), 10)]
}
\keyword{datasets}

