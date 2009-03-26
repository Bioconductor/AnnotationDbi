\name{@ANNOBJPREFIX@ENZYME2PROBE}
\alias{@ANNOBJPREFIX@ENZYME2PROBE}
\title{Map between Enzyme Commission Numbers and Manufacturer Identifiers}
\description{
  @ANNOBJPREFIX@ENZYME2PROBE is an R object that maps Enzyme Commission
  (EC) numbers to manufacturer identifiers.
}
\details{
  Each EC number maps to a named vector containing all of the
  manufacturer identifiers that correspond to the gene that produces that
  enzyme. The name of the vector corresponds to the EC number.
  
  Enzyme Commission numbers are assigned by the Nomenclature Committee
  of the International Union of Biochemistry and Molecular Biology
  \url{http://www.chem.qmw.ac.uk/iubmb/enzyme/} to allow enzymes to be
  identified. 
  
  An Enzyme Commission number is of the format EC x.y.z.w, where x, y, z,
  and w are numeric numbers. In @ANNOBJPREFIX@ENZYME2PROBE, EC is dropped from the
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
  
  Mappings were based on data provided by: @ENZYME2PROBESOURCE@  For fly, this
  required mapping these ids THROUGH the flybase IDs provided by NCBI.

}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
    # Convert to a list
    xx <- as.list(@ANNOBJPREFIX@ENZYME2PROBE)
   if(length(xx) > 0){
       # Get the probe identifiers for the first five enzyme
       #commission numbers
       xx[1:5]
       # Get the first one
       xx[[1]]
    }
}
\keyword{datasets}

