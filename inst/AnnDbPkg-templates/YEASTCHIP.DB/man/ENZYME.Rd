\name{@ANNOBJPREFIX@ENZYME}
\alias{@ANNOBJPREFIX@ENZYME}
\title{Map between Manufacturer IDs and Enzyme Commission (EC) Numbers}
\description{ 
  @ANNOBJPREFIX@ENZYME is an R object that provides mappings between
  manufacturer identifiers and EC numbers. 
}
\details{
  Each manufacturer identifier maps to a named vector containing the EC number that
  corresponds to the enzyme produced by that gene. The name corresponds
  to the manufacturer identifier. If this information is unknown, the vector
  will contain an \code{NA}.
  
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
  
  Mappings between probe identifiers and enzyme identifiers were obtained using files
  provided by: @ENZYMESOURCE@

}
\references{
\url{ftp://ftp.genome.ad.jp/pub/kegg/pathways}
}
\examples{
        x <- @ANNOBJPREFIX@ENZYME
        # Get the probe identifiers that are mapped to an EC number 
        mapped_probes <- mappedkeys(x)
        # Convert to a list
        xx <- as.list(x[mapped_probes])
        if(length(xx) > 0) {
          # Get the ENZYME for the first five probes
          xx[1:5]
          # For the first probe
          xx[[1]]
        }
}
\keyword{datasets}

