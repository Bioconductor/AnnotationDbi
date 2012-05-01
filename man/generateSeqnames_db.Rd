\name{generateSeqnames.db}

\alias{generateSeqnames.db}

\title{Generates the seqnames.db package and database}

\description{
  This function is used to generate the seqnames.db package and it's
  database from the csv files contained in the template for this package
  within AnnotationDbi.  The csv files are converted into database
  tables, and the DB is packaged into a new seqnames.db package.
}

\usage{
  generateSeqnames.db(version, outdir=".")
}

\arguments{
  \item{version}{Character. Version number for the final package.}
  \item{outdir}{Character. Path to output directory where the package is
  to be placed. By default the current working directory will be used.}
}

\details{
  The \code{generateSeqnames.db} function allows users to regenerate the
  seqnames.db package from csv sources contained in the currently
  installed AnnotationDbi package.  It is expected that the typical user
  will not need to use this at all, but in case they do, we have made it
  available.  We expect that the more common use case is someone who
  wants to make some new chromosome conventions available for the world.
  It is expected that this person will more typicalyl be charitable and
  want to share their conventions, so they could share their .csv files
  with us and we would add them to AnnotationDbi, install the updated
  package and then run this function to make a new package.

  The .csv files need to be formatted the same as the ones that are
  currently in the template in AnnotationDbi.  Examples of these .csv
  files can be found in AnnotationDbi in the
  "inst/seqnames-template/inst/extdata/dataFiles/" directory.  Each file
  must be named after it's corresponding genus and species with an
  underscore separator and a .csv file extension.  The 1st line of each
  file defines columns that are the names of the corresponding naming
  conventions.  And the chromosome names are then listed below this
  header line such that the equivalent names for the different styles
  share the same row.

  So for example the 1st four rows of Mus_musculus look like this (but
  with only one newline at the end of each row):

  
  UCSC,NCBI,ensembl
  
  chr1,1,1

  chr2,2,2

  chr3,3,3

  etc.

  
  Once you have your file ready your only need to place it in the same
  dir in AnnoationDbi (with the other files), install AnnotationDbi, and
  then run this function to generate a new seqnames.db package.  Of
  course, if you have a useful set of conventions or species to
  contribute, it would be best if you gave your .csv files to the
  Bioconductor core team so that we can add these files to the official
  version of AnnotationDbi and so that they can occur in the official
  seqnames.db package.
}

\value{
  A new seqnames.db package, complete with all the latest data stored in
  the dataFiles subdirectory
}

\author{Marc Carlson}

\examples{
\dontrun{
  generateSeqnames.db(version="1.0.0")
}
}

\keyword{manip}
