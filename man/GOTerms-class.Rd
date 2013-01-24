\name{GOTerms-class}
\docType{class}

\alias{class:GOTerms}
\alias{GOTerms-class}

\alias{GOTerms}

\alias{initialize,GOTerms-method}

% \alias{GOID}
\alias{GOID,GOTerms-method}
\alias{GOID,GOTermsAnnDbBimap-method}
\alias{GOID,character-method}
\alias{Term}
\alias{Term,GOTerms-method}
\alias{Term,GOTermsAnnDbBimap-method}
\alias{Term,character-method}
\alias{Ontology}
\alias{Ontology,GOTerms-method}
\alias{Ontology,GOTermsAnnDbBimap-method}
\alias{Ontology,character-method}
\alias{Definition}
\alias{Definition,GOTerms-method}
\alias{Definition,GOTermsAnnDbBimap-method}
\alias{Definition,character-method}
\alias{Synonym}
\alias{Synonym,GOTerms-method}
\alias{Synonym,GOTermsAnnDbBimap-method}
\alias{Synonym,character-method}
\alias{Secondary}
\alias{Secondary,GOTerms-method}
\alias{Secondary,GOTermsAnnDbBimap-method}
\alias{Secondary,character-method}

\alias{show,GOTerms-method}

\title{Class "GOTerms"}

\description{A class to represent Gene Ontology nodes}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{GOTerms(GOId, term, ontology, definition, synonym, secondary)}.
  GOId, term, and ontology are required.
}

\section{Slots}{
  \describe{
    \item{\code{GOID}:}{Object of class \code{"character"} A character
      string for the GO id of a primary node.}
    \item{\code{Term}:}{Object of class \code{"character"} A
      character string that defines the role of gene product
      corresponding to the primary GO id.}
    \item{\code{Ontology}:}{Object of class \code{"character"} Gene
      Ontology category. Can be MF - molecular function, CC - cellular
      component, or BP - biological process.}
    \item{\code{Definition}:}{Object of class \code{"character"} Further
      definition of the ontology of the primary GO id.}
    \item{\code{Synonym}:}{Object of class \code{"character"} other
      ontology terms that are considered to be synonymous to the primary
      term attached to the GO id (e.g. "type I programmed cell death" is a
      synonym of "apoptosis"). Synonymous here can mean that the
      synonym is an exact synonym of the primary term, is related to the
      primary term, is broader than the primary term, is more precise
      than the primary term, or name is related to the term, but is not
      exact, broader or narrower.}
    \item{\code{Secondary}:}{Object of class \code{"character"} GO ids
      that are secondary to the primary GO id as results of merging GO
      terms so that One GO id becomes the primary GO id and the rest
      become the secondary.}
  }
}

\section{Methods}{
  \describe{
    \item{GOID}{\code{signature(object = "GOTerms")}:
      The get method for slot GOID.}
    \item{Term}{\code{signature(object = "GOTerms")}:
      The get method for slot Term.}
    \item{Ontology}{\code{signature(object = "GOTerms")}:
      The get method for slot Ontology.}
    \item{Definition}{\code{signature(object = "GOTerms")}:
      The get method for slot Definition.}
    \item{Synonym}{\code{signature(object = "GOTerms")}:
      The get method for slot Synonym.}
    \item{Secondary}{\code{signature(object = "GOTerms")}:
      The get method for slot Secondary.}
    \item{show}{\code{signature(x = "GOTerms")}:
      The method for pretty print.}
  }
}

\references{\url{http://www.geneontology.org/}}

\seealso{
  \code{\link{makeGOGraph}} shows how to make GO mappings into graphNEL objects.
}

\note{GOTerms objects are used to represent primary GO nodes in the
   SQLite-based annotation data package GO.db}

\examples{
  gonode <- new("GOTerms", GOID="GO:1234567", Term="Test", Ontology="MF",
                          Definition="just for testing")
  GOID(gonode)
  Term(gonode)
  Ontology(gonode)

  ##Or you can just use these methods on a GOTermsAnnDbBimap
\dontrun{##I want to show an ex., but don't want to require GO.db
  require(GO.db)
  FirstTenGOBimap <- GOTERM[1:10] ##grab the 1st ten
  Term(FirstTenGOBimap)

  ##Or you can just use GO IDs directly
  ids = keys(FirstTenGOBimap)
  Term(ids)
}
}

\keyword{methods}
\keyword{classes}

