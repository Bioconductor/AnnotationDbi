\name{orgPackageName}

\alias{orgPackageName}

\title{Org package contained in annotation object}

\description{
  Get the name of the org package used by an annotation resource object.

  NOTE: This man page is for the \code{orgPackageName} \emph{S4 generic
  function} defined in the \pkg{AnnotationDbi} package.
  Bioconductor packages can define specific methods for annotation
  objects not supported by the default method.
}

\usage{
orgPackageName(x, ...)
}

\arguments{
  \item{x}{
    An annotation resource object.
  }
  \item{...}{
    Additional arguments.
  }
}

\value{
  A \code{character(1)} vector indicating the org package name.  

  Specific methods defined in Bioconductor packages should
  behave as consistently as possible with the default method.
}
