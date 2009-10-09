\name{KEGGFrame}
\docType{class}

% Classes

\alias{KEGGFrame}
\alias{class:KEGGFrame}
\alias{KEGGFrame-class}


% methods
\alias{KEGGFrame}
\alias{KEGGFrame,data.frame,character-method}
\alias{KEGGFrame,data.frame,missing-method}

\alias{getKEGGFrameData}
\alias{getKEGGFrameData,KEGGFrame-method}
\alias{getKEGGFrameData,KEGGAllFrame-method}


\title{KEGGFrame objects}

\description{
  These objects each contain a data frame which is required to be
  composed of 2 columns. The 1st column are KEGG IDs.  The second are
  the gene IDs that match to the KEGG IDs.  There is also a slot for the
  organism that these anotations pertain to.  \code{getKEGGFrameData} is
  just an accessor method and returns the data.frame contained in the
  KEGGFrame object and is mostly used by other code internally.
}


\details{
  The purpose of these objects is to create a safe way for annotation
  data about KEGG from non-traditional sources to be used for analysis
  packages like GSEABase and eventually Category.
}

\examples{
  ## Make up an example
  genes = c(2,9,9,10)
  KEGGIds = c("04610","00232","00983","00232")
  frameData = data.frame(cbind(KEGGIds,genes))

  library(AnnotationDbi)
  frame=KEGGFrame(frameData,organism="Homo sapiens")

  getKEGGFrameData(frame)
}

\keyword{classes}
\keyword{interface}
