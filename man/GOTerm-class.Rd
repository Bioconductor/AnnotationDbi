\name{GOTerm-class}
\docType{class}

\alias{class:GOTerm}
\alias{GOTerm-class}

\alias{GOTerm}

\alias{GOID}
\alias{Term}
\alias{Ontology}
\alias{Synonym}
\alias{Secondary}
\alias{Definition}

\alias{GOID,GOTerm-method}
\alias{Term,GOTerm-method}
\alias{Ontology,GOTerm-method}
\alias{Synonym,GOTerm-method}
\alias{Secondary,GOTerm-method}
\alias{Definition,GOTerm-method}

\alias{show,GOTerm-method}

\title{Class "GOTerm"}

\description{A class to represent Gene Ontology terms for a given node}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("GOTerm",
    GOId, term, ontology, synonym, secondary, definition)}.
  GOId, term, and ontology are required.
}

\section{Slots}{
  \describe{
    \item{\code{GOID}:}{Object of class \code{"character"} A character
      stirng for the GO id of a primary node}
    \item{\code{Term}:}{Object of class \code{"character"} A
      character string that defines the role of gene product
      corresponding to the primary GO id}
    \item{\code{Ontology}:}{Object of class \code{"character"} Gene
      Ontology category. Can be MF - molecular function, CC - cellular
      component, or BP - biological process}
    \item{\code{Synonym}:}{Object of class \code{"character"} other
      ontology terms that are considered to be synonymous to the primary
      term attached to the GO id (e.g. type I programmed cell death is a
      synonym of apoptosis). Synonymous here can mean that the
      synonym is an exact synonym of the primary term, is related to the
      primary term, is broader than the primary term, is more precise
      than the primary term, or  name is related to the term, but is not
      exact, broader or narrower.}
    \item{\code{Secondary}:}{Object of class \code{"character"} GO ids
      that are secondary to the primary GO id as results of merging GO
      terms so that One GO id becomes the primary GO id and the rest
      become the secondary}
    \item{\code{Definition}:}{Object of class \code{"character"} Further
      definition of the ontology of the primary GO id}
  }
}

\section{Methods}{
  \describe{
    \item{GOID}{\code{signature(object = "GOTerm")}:
      The get method for slot GOID}
    \item{Term}{\code{signature(object = "GOTerm")}:
      The get method for slot Term}
    \item{Ontology}{\code{signature(object = "GOTerm")}:
      The get method for slot Ontology}
    \item{Synonym}{\code{signature(object = "GOTerm")}:
      The get method for slot Synonym}
    \item{Secondary}{\code{signature(object = "GOTerm")}:
      The get method for slot Secondary}
    \item{Definition}{\code{signature(object = "GOTerm")}:
      The get method for slot Definition}
    \item{print}{\code{signature(x = "GOTerm")}:
      The method for pretty print}
  }
}

\references{\url{http://www.geneontology.org/}}

\note{GOTerm objects are used to represent GO terms for primary GO
  nodes in the SQLite-based annotation data package GOdb}

\examples{
  goterm <- new("GOTerm", GOID="GO:1234567", Term="Test", Ontology="MF")
  GOID(goterm)
  Term(goterm)
  Ontology(goterm)
}

\keyword{methods}
\keyword{classes}

