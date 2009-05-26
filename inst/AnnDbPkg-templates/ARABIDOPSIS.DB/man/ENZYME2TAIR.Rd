\name{@ANNOBJPREFIX@ENZYME2TAIR}
\alias{@ANNOBJPREFIX@ENZYME2TAIR}
\title{Map between Enzyme Commission Numbers and TAIR Identifiers}
\description{
  @ANNOBJPREFIX@ENZYME2TAIR is an R object that maps Enzyme Commission
  (EC) numbers to TAIR identifiers.
}
\details{
  Each EC number maps to a named vector containing all of the
  TAIR identifiers that correspond to the gene that produces that
  enzyme. The name of the vector corresponds to the EC number.
  
  Enzyme Commission numbers are assigned by the Nomenclature Committee
  of the International Union of Biochemistry and Molecular Biology
  \url{http://www.chem.qmw.ac.uk/iubmb/enzyme/} to allow enzymes to be
  identified. 
  
  An Enzyme Commission number is of the format EC x.y.z.w, where x, y, z,
  and w are numeric numbers. In @ANNOBJPREFIX@ENZYME2TAIR, EC is dropped from the
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
  
  Mappings were based on data provided by: @ENZYME2TAIRSOURCE@

}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ENZYME2TAIR)
   if(length(xx) > 0){
       # Gets the tair identifiers for the first five enzyme
       #commission numbers
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}

