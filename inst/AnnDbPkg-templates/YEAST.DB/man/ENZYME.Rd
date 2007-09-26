\name{@ANNOBJPREFIX@ENZYME}
\alias{@ANNOBJPREFIX@ENZYME}
\alias{@ANNOBJPREFIX@ENZYME2ORF}
\title{Map between ORF IDs and Enzyme Commission (EC) Numbers}
\description{ 
  @ANNOBJPREFIX@ENZYME is an R object that provides mappings between
  ORF identifiers and EC numbers. 
}
\details{
  Each ORF identifier maps to a named vector containing the EC number that
  corresponds to the enzyme produced by that gene. The name corresponds
  to the ORF identifier. If this information is unknown, the vector
  will contain an \code{NA}.
  
  Enzyme Commission numbers are assigned by the Nomenclature Committee
  of the International Union of Biochemistry and Molecular Biology
  \url{http://www.chem.qmw.ac.uk/iubmb/enzyme/} to allow enzymes to be
  identified. 
  
  An Enzyme Commission number is of the format EC x.y.z.w, where x, y, z,
  and w are numeric numbers. In @ANNOBJPREFIX@ENZYME2ORF, EC is dropped from the
  Enzyme Commission numbers.

  Enzyme Commission numbers have corresponding names that describe the
  functions of enzymes in such a way that EC x is a more general
  description than EC x.y that in turn is a more general description than
  EC x.y.z. The top level EC numbers and names are listed below:

  EC 1 oxidoreductases
  
  EC 2 transferases

  EC 3 hydrolases

  EC 4 lyases
  
  EC 5 isomerases
  
  EC 6 ligases
  
  The EC name for a given EC number can be viewed at
  \url{http://www.chem.qmul.ac.uk/iupac/jcbn/index.html#6} 
  
  Mappings between ORF identifiers and enzyme identifiers were obtained using files
  provided by: @ENZYMESOURCE@

  
  For the reverse map, each EC number maps to a named vector containing
  the ORF identifier that corresponds to the gene that produces
  that enzyme. The name of the vector corresponds to the EC number.
}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
    x <- @ANNOBJPREFIX@ENZYME
    # Get the ORF identifiers that are mapped to an EC number 
    mapped_genes <- mappedkeys(x)
    # Convert to a list
    xx <- as.list(x[mapped_genes])
    if(length(xx) > 0) {
      # Get the ENZYME id for the first five genes
      xx[1:5]
      # Get the first one
      xx[[1]]
    }
    # For the reverse map:
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ENZYME2ORF)
    if(length(xx) > 0){
       # Gets the ORF identifiers for the first five enzyme
       #commission numbers
       xx[1:5]
       # Get the first one
       xx[[1]]
    }

}
\keyword{datasets}
