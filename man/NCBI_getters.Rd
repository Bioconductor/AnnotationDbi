\name{buildEntrezGeneDb}
\alias{buildEntrezGeneDb}
\alias{getEntrezGenesFromTaxId}


\title{builds an Entrez Gene centric organism database}

\description{
  These functions contact the NCBI eutils web services and then
  construct an organism (org.Xx.eg.db) database.  
}

\usage{
  buildEntrezGeneDb(entrezGenes, file="test.sqlite")
  getEntrezGenesFromTaxId(taxId)
}

\arguments{
  \item{entrezGenes}{
    character vector of entrez Gene IDs that you want annotated.  We
    presently check to make sure these are from the same species.  You
    will get an error if they aren't.
  }
  \item{file}{
    file to write the database to.  Traditionally these end with the
    .sqlite extension.
  }
  \item{taxId}{
    A taxonomy ID from NCBI.  You can browse these to select the species
    of interest from their website.
  }
}

\value{
  In the case of \code{getEntrezGenesFromTaxId}, this will be a
  character vector of entrez gene IDs.  In the case of
  \code{buildEntrezGeneDb}, then you will produce a SQLite database.
}

\examples{
  ## seems that some of the EGs are not from the taxid specified? (more tests)
  ## EGs <- getEntrezGenesFromTaxId(taxId)
  ## temporarily, lets use known EGs to run tests
  library(org.Hs.eg.db)
  EGs <- Lkeys(org.Hs.egCHR)
  EGs <- EGs[1:4] ## Obviously NOT a typical step.
  buildEntrezGeneDb(EGs, file=tempfile())
}

\keyword{interface}
