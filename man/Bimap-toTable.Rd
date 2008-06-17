\name{Bimap-toTable}

\alias{Bimap-toTable}

\alias{toTable}
\alias{toTable,FlatBimap-method}
\alias{toTable,AnnDbBimap-method}

\alias{nrow}
\alias{nrow,FlatBimap-method}
\alias{nrow,AnnDbTable-method}
\alias{nrow,AnnDbBimap-method}
\alias{nrow,Go3AnnDbBimap-method}

\alias{ncol}
\alias{ncol,Bimap-method}

\alias{dim,Bimap-method}

\alias{head}
\alias{head,FlatBimap-method}

\alias{tail}
\alias{tail,FlatBimap-method}

\alias{links}
\alias{links,FlatBimap-method}
\alias{links,AnnDbBimap-method}
\alias{links,Go3AnnDbBimap-method}

\alias{count.links}
\alias{count.links,Bimap-method}
\alias{count.links,Go3AnnDbBimap-method}

\alias{nhit}
\alias{nhit,Bimap-method}
\alias{nhit,environment-method}
\alias{nhit,list-method}

\alias{colnames}
\alias{colnames,FlatBimap-method}
\alias{colnames,AnnDbBimap-method}

\alias{colmetanames}
\alias{colmetanames,FlatBimap-method}
\alias{colmetanames,AnnDbBimap-method}

\alias{Lkeyname}
\alias{Lkeyname,Bimap-method}
\alias{Lkeyname,AnnDbBimap-method}
\alias{Rkeyname}
\alias{Rkeyname,Bimap-method}
\alias{Rkeyname,AnnDbBimap-method}
\alias{keyname}
\alias{keyname,Bimap-method}

\alias{tagname}
\alias{tagname,Bimap-method}
\alias{tagname,AnnDbBimap-method}

\alias{Rattribnames}
\alias{Rattribnames,Bimap-method}
\alias{Rattribnames,AnnDbBimap-method}

\alias{Rattribnames<-}
\alias{Rattribnames<-,FlatBimap-method}
\alias{Rattribnames<-,AnnDbBimap-method}
\alias{Rattribnames<-,Go3AnnDbBimap-method}


\title{Methods for manipulating a Bimap object in a data-frame style}

\description{
  These methods are part of the \link{Bimap} interface
  (see \code{?\link{Bimap}} for a quick overview of the \link{Bimap}
  objects and their interface).
}

\usage{
  ## Extract all the columns of the map (links + right attributes)
  toTable(x)
  nrow(x)
  ncol(x)
  #dim(x)
  head(x, ...)
  tail(x, ...)

  ## Extract only the links of the map
  links(x)
  count.links(x)
  nhit(x)

  ## Col names and col metanames
  colnames(x, do.NULL=TRUE, prefix="col")
  colmetanames(x)
  Lkeyname(x)
  Rkeyname(x)
  keyname(x)
  tagname(x)
  Rattribnames(x)
  Rattribnames(x) <- value
}

\arguments{
  \item{x}{
    A \link{Bimap} object (or a list or an environment for \code{nhit}).
  }
  \item{...}{
    Further arguments to be passed to or from other methods (see
    \code{\link[utils]{head}} or \code{\link[utils]{tail}}
    for the details).
  }
  \item{do.NULL}{
    Ignored.
  }
  \item{prefix}{
    Ignored.
  }
  \item{value}{
    A character vector containing the names of the new right attributes
    (must be a subset of the current right attribute names)
    or NULL.
  }
}

\details{
  \code{toTable(x)} turns \link{Bimap} object \code{x} into a
  data frame (see section "Flat representation of a bimap" in
  \code{?\link{Bimap}} for a short introduction to this concept).
  For simple maps (i.e. no tags and no right attributes),
  the resulting data frame has only 2 columns, one for the left
  keys and one for the right keys, and each row in the data frame
  represents a link (or edge) between a left and a right key.
  For maps with tagged links (i.e. a tag is associated to each
  link), \code{toTable(x)} has one additional colmun for the tags
  and there is still one row per link.
  For maps with right attributes (i.e. a set of attributes is
  associated to each right key), \code{toTable(x)} has one
  additional colmun per attribute. So for example if \code{x} has
  tagged links and 2 right attributes, \code{toTable(x)} will
  have 5 columns: one for the left keys, one for the right keys,
  one for the tags, and one for each right attribute (always the
  rightmost columns).
  Note that if at least one of the right attributes is multivalued
  then more than 1 row can be needed to represent the same link
  so the number of rows in \code{toTable(x)} can be strictly
  greater than the number of links in the map.

  \code{nrow(x)} is equivalent to (but more efficient than)
  \code{nrow(toTable(x))}.

  \code{ncol(x)} is equivalent to (but more efficient than)
  \code{ncol(toTable(x))}.

  \code{colnames(x)} is equivalent to (but more efficient than)
  \code{colnames(toTable(x))}. Columns are named accordingly to
  the names of the SQL columns where the data are coming from.
  An important consequence of this that they are not necessarily
  unique.

  \code{colmetanames(x)} returns the metanames for the column of
  \code{x} that are not right attributes. Valid column metanames
  are \code{"Lkeyname"}, \code{"Rkeyname"} and \code{"tagname"}.

  \code{Lkeyname}, \code{Rkeyname}, \code{tagname} and
  \code{Rattribnames} return the name of the column (or columns)
  containing the left keys, the right keys, the tags and the right
  attributes, respectively.

  Like \code{toTable(x)}, \code{links(x)} turns \code{x} into a
  data frame but the right attributes (if any) are dropped.
  Note that dropping the right attributes produces a data frame
  that has eventually less columns than \code{toTable(x)}
  and also eventually less rows because now exactly 1 row is
  needed to represent 1 link.

  \code{count.links(x)} is equivalent to (but more efficient than)
  \code{nrow(links(x))}.

  \code{nhit(x)} returns a named integer vector indicating the
  number of "hits" for each key in \code{x} i.e. the number of links
  that start from each key.
}

\value{
  A data frame for \code{toTable} and \code{links}.

  A single integer for \code{nrow}, \code{ncol} and \code{count.links}.

  A character vector for \code{colnames}, \code{colmetanames}
  and \code{Rattribnames}.

  A character string for \code{Lkeyname}, \code{Rkeyname}
  and \code{tagname}.

  A named integer vector for \code{nhit}.
}

\author{H. Pages}

\seealso{
  \link{Bimap},
  \link{BimapFormatting},
  \link{AnnDbBimap-envirAPI}
}

\examples{
  library(GO.db)
  x <- GOSYNONYM
  x
  toTable(x)[1:4, ]
  toTable(x["GO:0007322"])
  links(x)[1:4, ]
  links(x["GO:0007322"])

  nrow(x)
  ncol(x)
  dim(x)
  colnames(x)
  colmetanames(x)
  Lkeyname(x)
  Rkeyname(x)
  tagname(x)
  Rattribnames(x)

  links(x)[1:4, ]
  count.links(x)

  y <- GOBPCHILDREN
  nhy <- nhit(y) # 'nhy' is a named integer vector
  identical(names(nhy), keys(y)) # TRUE
  table(nhy)
  sum(nhy == 0) # number of GO IDs with no children
  names(nhy)[nhy == max(nhy)] # the GO ID(s) with the most direct children

  ## Some sanity check
  sum(nhy) == count.links(y) # TRUE

  ## Changing the right attributes of the GOSYNONYM map (advanced
  ## users only)
  class(x) # GOTermsAnnDbBimap
  as.list(x)[1:3]
  colnames(x)
  colmetanames(x)
  tagname(x) # untagged map
  Rattribnames(x)
  Rattribnames(x) <- Rattribnames(x)[3:1]
  colnames(x)
  class(x) # AnnDbBimap
  as.list(x)[1:3]
}

\keyword{methods}
