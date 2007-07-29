\name{@ANNOBJPREFIX@PATHID2EXTID}
\alias{@ANNOBJPREFIX@PATHID2EXTID}
\title{An annotation data object that maps KEGG pathway identifiers to Entrez Gene
  or Open Reading Frame identifiers.}
\description{
  @ANNOBJPREFIX@PATHID2EXTID maps KEGG pathway identifiers to Entrez Gene (for human,
  mouse, and rat) or Open Reading Frame (for yeast) identifiers   
}
\details{
  This is an R object containing key and value pairs. Keys are
  KEGG pathway identifiers and values are Entrez Gene identifiers for human, mouse, and
  rat or Open Reading Frame (ORF) identifiers for yeast. Values are
  vectors of length 1 or greater depending on whether a pathway identifier can
  be maapped to one or more Entrez Gene or ORF identifiers. NAs are
  assigned to KEGG pathway identifiers that can not be mapped to any Entrez Gene
  or ORF identifiers. 

  KEGG pathway identifiers are the identifiers used by KEGG for various
  pathways.

  Mappings between KEGG pathway identifiers and pathway names can be obtained
  through another annotation data object named @ANNOBJPREFIX@PATHID2NAME. 

  Mappings were based on data provided by: @PATHID2EXTIDSOURCE@

}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
	xx <- as.list(@ANNOBJPREFIX@PATHID2EXTID)
	if(length(xx) > 0){
		# Get the value of the first key
		xx[[1]]
		# Get the values for multiget for a few keys
		if(length(xx) >= 3){
			xx[1:3]
		}
	}
}
\keyword{datasets}

