\name{GOFrame}
\docType{class}

% Classes

\alias{GOFrame}
\alias{class:GOFrame}
\alias{GOFrame-class}

\alias{GOAllFrame}
\alias{class:GOAllFrame}
\alias{GOAllFrame-class}


% methods
\alias{GOFrame}
\alias{GOFrame,data.frame,character-method}
\alias{GOFrame,data.frame,missing-method}

\alias{GOAllFrame}
\alias{GOAllFrame,GOFrame-method}

\alias{getGOFrameData}
\alias{getGOFrameData,GOFrame-method}
\alias{getGOFrameData,GOAllFrame-method}


\title{GOFrame and GOAllFrame objects}

\description{
  These objects each contain a data frame which is required to be
  composed of 3 columns. The 1st column are GO IDs.  The second are
  evidence codes and the 3rd are the gene IDs that match to the GO IDs
  using those evidence codes.  There is also a slot for the organism
  that these anotations pertain to.
}


\details{
  The GOAllFrame object can only be generated from a GOFrame object and
  its contructor method does this automatically from a GOFrame
  argument.  The purpose of these objects is to create a safe way for
  annotation data about GO from non-traditional sources to be used for
  analysis packages like GSEABase and eventually GOstats.
}

\examples{
  ## Make up an example
  genes = c(1,10,100)
  evi = c("ND","IEA","IDA")
  GOIds = c("GO:0008150","GO:0008152","GO:0001666")
  frameData = data.frame(cbind(GOIds,evi,genes))

  library(AnnotationDbi)
  frame=GOFrame(frameData,organism="Homo sapiens")
  allFrame=GOAllFrame(frame)

  getGOFrameData(allFrame)
}

\keyword{classes}
\keyword{interface}
