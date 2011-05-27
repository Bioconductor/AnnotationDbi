\name{makeOrgPackageFromNCBI}

\alias{makeOrgPackageFromNCBI}
\alias{makeOrgPackageFromBiomart}


\title{
  Making an organism package from annotations available from
  NCBI.
}
\description{
  The \code{makeOrgPackageFromNCBI} function allows the user to make an
  organism package from NCBI annotations available from the NCBI.
}
\usage{

  makeOrgPackageFromNCBI(
    version=,
    maintainer,
    author,
    outputDir=".",
    tax_id,
    genus,
    species)
}

\arguments{
  \item{version}{What is the version number for this package?}
  \item{maintainer}{Who is the package maintainer? (must include email to be 
    valid)}
  \item{author}{Who is the creator of this package?}
  \item{outputDir}{A path where the package source should be assembled.}
  \item{tax_id}{The Taxonomy ID that represents your organism.  (NCBI has
  a nice online browser for finding the one you need)}
  \item{genus}{Single string indicating the genus.}
  \item{species}{Single string indicating the species.}
}


\value{Nothing returned to the R session. Just creates an organism
annotation package.}

\author{
  M. Carlson
}


\examples{

\dontrun{
## Makes an organism package for Zebra Finch from NCBI:

makeOrgPackageFromNCBI(version = "0.1",
                       author = "Some One <so@someplace.org>",
                       maintainer = "Some One <so@someplace.org>",
                       outputDir = ".",
                       tax_id = "59729",
                       genus = "Taeniopygia",
                       species = "guttata")

}

}
