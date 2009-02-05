\name{@PKGNAME@}
\docType{data}
\alias{@PKGNAME@}
\title{Probe sequence for microarrays of type @ARRAYTYPE@.}
\description{This data object was automatically created by the @CREATOR@.}
\usage{data(@PKGNAME@)}
\format{A data frame with @NROW@ rows and @NCOL@ columns, as follows.
  \tabular{lll}{
    sequence \tab character \tab probe sequence \cr
    x  \tab integer  \tab x-coordinate on the array \cr
    y  \tab integer  \tab y-coordinate on the array \cr
    Probe.Set.Name      \tab character \tab Affymetrix Probe Set Name \cr
    Probe.Interrogation.Position \tab integer \tab Probe Interrogation Position \cr
    Target.Strandedness \tab factor \tab Target Strandedness\cr
}}

\source{@DATASOURCE@}

\keyword{datasets}
\examples{
  @PKGNAME@
  as.data.frame(@PKGNAME@[1:3,])
}
