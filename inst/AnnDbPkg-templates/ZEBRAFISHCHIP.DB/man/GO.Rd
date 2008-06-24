\name{@ANNOBJPREFIX@GO}
\alias{@ANNOBJPREFIX@GO}
\title{Map between Manufacturer IDs and Gene Ontology (GO)}
\description{
  @ANNOBJPREFIX@GO is an R object that provides mappings between
  manufacturer identifiers and the GO identifiers that they are 
  directly associated with. 
}
\details{
     Each Entrez Gene identifier is mapped to a list of lists.  The names on
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
  
  Mappings between probe identifiers and GO information were obtained through
  their mappings to Entrez Gene identifiers. NAs are assigned to probe identifiers
  that can not be mapped to any Gene Ontology information. Mappings
  between Gene Ontology identifiers an Gene Ontology terms and other information
  are available in a separate data package named GO.
  
  Mappings were based on data provided by: @GOSOURCE@

}

\examples{
        x <- @ANNOBJPREFIX@GO
        # Get the probe identifiers that are mapped to a GO ID
        mapped_probes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_probes])
        if(length(xx) > 0) {
            # Try the firest one
            got <- xx[[1]]           
            got[[1]][["GOID"]]
            got[[1]][["Ontology"]]
            got[[1]][["Evidence"]]

        }
}
\keyword{datasets}

