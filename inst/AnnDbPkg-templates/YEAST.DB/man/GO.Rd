\name{@ANNOBJPREFIX@GO}
\alias{@ANNOBJPREFIX@GO}
\alias{@ANNOBJPREFIX@GO2ORF}
\title{Map between ORF IDs and Gene Ontology (GO)}
\description{
  @ANNOBJPREFIX@GO is an R object that provides mappings between
  ORF identifiers and the GO identifiers that they are 
  directly associated with. This mapping and its reverse mapping do NOT
  associate the child terms from the GO ontology with the gene.  Only
  the directly evidenced terms are represented here.
}
\details{
     Each ORF identifier is mapped to a list of lists.  The names on
     the outer list are GO identifiers.  Each inner list consists of three
     named elements: GOID, Ontology, and Evidence.

     The GOID element matches the GO identifier named in the outer list and is
     included for convenience when processing the data using 'lapply'.

     The Ontology element indicates which of the three Gene Ontology
     categories this identifier belongs to.  The categories are
     biological process (BP), cellular component (CC), and molecular
     function (MF).

     The Evidence element contains a code indicating what kind of
     evidence supports the association of the GO identifier to the Entrez Gene
     id. The evidence codes in use include:

  IMP: inferred from mutant phenotype  

  IGI: inferred from genetic interaction

  IPI: inferred from physical interaction  

  ISS: inferred from sequence similarity 

  IDA: inferred from direct assay  

  IEP: inferred from expression pattern  

  IEA: inferred from electronic annotation  

  TAS: traceable author statement  

  NAS: non-traceable author statement  

  ND: no biological data available  

  IC: inferred by curator
  
  Mappings between ORF identifiers and GO information were obtained through
  their mappings to ORF identifiers. NAs are assigned to ORF identifiers
  that can not be mapped to any Gene Ontology information. Mappings
  between Gene Ontology identifiers an Gene Ontology terms and other information
  are available in a separate data package named GO.
  
  Mappings were based on data provided by: @GOSOURCE@

  
  For the reverse map @ANNOBJPREFIX@GO2ORF, each GO term maps to a named vector of
  ORF identifiers. A GO identifier may be mapped to the same
  ORF identifier more than once but the evidence code can be
  different. Mappings between Gene Ontology identifiers and Gene Ontology
  terms and other information are available in a separate data package
  named GO.
}
\seealso{
  \code{\link{@ANNOBJPREFIX@GO2ALLORFS}}.
}
\references{
\url{ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/}
}
\examples{
    x <- @ANNOBJPREFIX@GO
    # Get the ORF identifiers that are mapped to a GO ID
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
        # Try the first one
        got <- xx[[1]]           
        got[[1]][["GOID"]]
        got[[1]][["Ontology"]]
        got[[1]][["Evidence"]]
    }
    # For the reverse map:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@GO2ORF)
    if(length(xx) > 0){
        # Gets the ORF ids for the top 2nd and 3nd GO identifiers
        goids <- xx[2:3]
        # Gets the ORF ids for the first element of goids
        goids[[1]]
        # Evidence code for the mappings
        names(goids[[1]])
    }
}
\keyword{datasets}
