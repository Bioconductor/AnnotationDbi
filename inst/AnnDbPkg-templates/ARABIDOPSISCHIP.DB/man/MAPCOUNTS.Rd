\name{@ANNOBJPREFIX@MAPCOUNTS} 
\alias{@ANNOBJPREFIX@MAPCOUNTS}
\title{Quality control information for @ANNOBJPREFIX@}
\description{
   @ANNOBJPREFIX@MAPCOUNTS is an R object that provides quality control
   information for @ANNOBJPREFIX@
}
\details{
  This file contains quality control information that can be displayed
  by typing @ANNOBJPREFIX@() after loading the package using
  library(@ANNOBJPREFIX@). The follow items are included: 

  Date built: Date when the package was built.

  Number of probes: total number of probes included

  Probe number missmatch: if the total number of probes of any of the
  data file is different from a base file used to check the data files
  the name of the data file will be listed 

  Probe missmatch: if any of probes in a data file missmatched that of
  the base file,  the name of the data file will be listed 

  Mappings found for probe based files: number of mappings obtained for
  the total number of probes 

 Mappings found for non-probe based files: total number of mappings obtained
}
\keyword{datasets}

