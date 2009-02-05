\name{makeProbePackage}
\alias{makeProbePackage}
\title{Make a package with probe sequence related data for microarrays}
\description{Make a package with probe sequence related data for microarrays}
\usage{
makeProbePackage(arraytype,
    importfun = "getProbeDataAffy",
    maintainer,
    version,
    species,
    pkgname = NULL,
    outdir  = ".",
    force = FALSE, quiet = FALSE,
    check = TRUE, build = TRUE, unlink = TRUE, \dots)
}
\arguments{
  \item{arraytype}{Character. Name of array type (typically a vendor's
    name like "HG-U133A").}
  \item{importfun}{Character. Name of a function that can read the
    probe sequence data e.g. from a file. See
    \code{\link{getProbeDataAffy}} for
    an example.}
  \item{maintainer}{Character. Name and email address of the maintainer.}
  \item{version}{Character. Version number for the package.}
  \item{species}{Character. Species name in the format Genus\_species
    (e.g., Homo\_sapiens)}
  \item{pkgname}{Character. Name of the package. If missing, a name is
    created from \code{arraytype}.}
  \item{outdir}{Character. Path where the package is to be written.}
  \item{force}{Logical. If \code{TRUE} overrides possible warnings}
  \item{quiet}{Logical. If \code{TRUE} do not print statements on progress on the console}
  \item{check}{Logical. If \code{TRUE} call \code{R CMD check} on the package}
  \item{build}{Logical. If \code{TRUE} call \code{R CMD build} on the package}
  \item{unlink}{Logical. If \code{TRUE} unlink (remove) the \code{check}
    directory (only relevant if \code{check=TRUE})}
  \item{\dots}{Further arguments that get passed along to \code{importfun}}
}
\details{See vignette.

Important note for \emph{Windows} users: Building and checking packages
requires some tools outside of \R (e.g. a Perl interpreter).
While these tools are standard with practically every Unix, they do not
come with MS-Windows and need to be installed separately on your computer.
See \url{http://www.murdoch-sutherland.com/Rtools}.
If you just want to build probe packages, you will not need the
compilers, and the "Windows help" stuff is optional.
}

\examples{
filename <- system.file("extdata", "HG-U95Av2_probe_tab.gz", 
     package="AnnotationDbi")
outdir   <- tempdir()
me       <- "Wolfgang Huber <huber@ebi.ac.uk>"
makeProbePackage("HG-U95Av2",
                 datafile   = gzfile(filename, open="r"),
                 outdir     = outdir,
                 maintainer = me, 
                 version    = "0.0.1",
                 species    = "Homo_sapiens", 
		 check      = FALSE,
                 force      = TRUE)
dir(outdir)
}
\keyword{IO}
\keyword{utilities}
