\name{@ANNOBJPREFIX@PMID2EG}
\alias{@ANNOBJPREFIX@PMID2EG}
\title{Map between PubMed Identifiers and Entrez Gene Identifiers}
\description{
  @ANNOBJPREFIX@PMID2EG is an R object that provides mappings between
  PubMed identifiers and entrez gene identifiers.
}
\details{
  Each PubMed identifier is mapped to a named vector of entrez gene identifiers. The
  name represents the PubMed identifier and the vector contains all entrez gene
  identifiers that are represented by that PubMed identifier. The length of the vector
  may be one or longer, depending on how many entrez gene identifiers are
  mapped to a given PubMed identifier.

  Titles, abstracts, and possibly full texts of articles can be obtained
  from PubMed by providing a valid PubMed identifier. The pubmed function of
  annotate can also be used for the same purpose
  
  Mappings were based on data provided by: @PMID2EGSOURCE@
  
}

\examples{
    # Convert the object to a list
    xx <- as.list(@ANNOBJPREFIX@PMID2EG)
    if(length(xx) > 0){
        # The entrez gene identifiers for the first two elements of XX
        xx[1:2]
        # Get the first one
        xx[[1]]
        if(interactive() && require(annotate)){
            # Gets article information as XML files for a PubMed id
            xmls <- pubmed(names(xx)[1], disp = "data")
            # Views article information using a browser
            pubmed(names(xx)[1], disp = "browser")
        }
    }
}

\keyword{datasets}

