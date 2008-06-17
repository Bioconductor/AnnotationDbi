\name{Bimap-toTable}

\alias{Bimap-toTable}

\alias{toTable}
\alias{toTable,FlatBimap-method}
\alias{toTable,AnnDbBimap-method}

\alias{head}
\alias{head,FlatBimap-method}

\alias{tail}
\alias{tail,FlatBimap-method}

\alias{links}
\alias{links,FlatBimap-method}
\alias{links,AnnDbBimap-method}
\alias{links,Go3AnnDbBimap-method}

\alias{nrow}
\alias{nrow,FlatBimap-method}
\alias{nrow,AnnDbTable-method}
\alias{nrow,AnnDbBimap-method}
\alias{nrow,Go3AnnDbBimap-method}

\alias{dim,Bimap-method}

\alias{count.links}
\alias{count.links,Bimap-method}
\alias{count.links,Go3AnnDbBimap-method}

\alias{nhit}
\alias{nhit,Bimap-method}
\alias{nhit,environment-method}
\alias{nhit,list-method}


\title{Methods for extracting data from a Bimap object}

\description{
  These methods are part of the \link{Bimap} interface
  (see \code{?\link{Bimap}} for a quick overview of the \link{Bimap}
  objects and their interface).
}

\usage{
  ## Extract the data
  toTable(x)
  head(x, ...)
  tail(x, ...)
  links(x)

  ## Some basic counting on the data without actually extracting
  ## them
  nrow(x)
  #dim(x)
  count.links(x)
  nhit(x)
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
}

\details{


  Except for \code{nhit}, all these methods are undirected method

  The function \code{links()} will in many cases give very similar
  output to the function \code{toTable()}, but these functions are not
  the same.  Both of these functions aim to describe the data that is in
  a \link{Bimap} object, but while the \code{toTable()} function will
  output all of the data in the map, the \code{links()} function will
  only output the data that is tied directly to the links
  \preformatted{(edges)} of the \link{Bimap}.

  To understand exactly what this means, it is important to consider
  that a Bimap is composed of 3 different kinds of elements.  The Lkeys
  and Rkeys style of element define the left and right values that
  compose the basic \link{Bimap} mapping.  In addition to the Lkeys and
  Rkeys, it is also possible to label the edges themselves with a
  tagname variable.  This tagname is the second kind of element and is
  meant to represent data that about the relationship between a pair of
  keys.  A classic example of this would be an evidence code used to map
  a GO term to a gene or probe.  Finally, the third kind of element is
  the attribute \preformatted{(Lattribute or Rattribute)} which can be
  used to label additional information onto the Lkeys or Rkeys.  This
  third kind of element is meant to capture information about the
  individual Lkeys and Rkeys that is independent of the relationship
  between a pair of keys such as the ontology that a term was gathered
  from.

  Therefore, the \code{links()} function will output the Lkeys, and
  Rkeys along with any tagname values but leave out any of the attribute
  information.

  Related to these functions, the \code{count.links()} function will
  return only the total number of edges in a \link{Bimap}.  This might
  be more useful than the \code{nrows()} function which just gives you
  the number of rows you would get if you had called \code{toTable()}.
  The reason \code{count.links()} might be better is for Bimaps where
  there are multiple right attributes attached to an individual link.
  In these cases, \code{nrows()} will count many rows that represent the
  same essential relationship between the left and right keys, whereas
  \code{count.links()} will just give you one count for each
  relationship.

  Finally, \code{nhit()} returns a named integer vector containing the
  number of hits for every left key in a \link{Bimap}.  The number of
  hits for a given left key is equal to the number of links
  \preformatted{(edges)} mapping from that key to a corresponding right
  key.  This function is useful for determining whether or not a map is
  a many to one or a many to many map.
}

\value{
  The \code{toTable()} and \code{links()} functions return dataframes.

  The \code{count.links()} and  \code{nrow()} functions return integers.

  The \code{nhit()} function returns a named integer vector
  corresponding to the number of hits \preformatted{(or right keys)} for
  each left key.
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
  dim(x)
  count.links(x)

  y <- GOBPCHILDREN
  nhy <- nhit(y) # 'nhy' is a named integer vector
  identical(names(nhy), keys(y)) # TRUE
  table(nhy)
  sum(nhy == 0) # number of GO IDs with no children
  names(nhy)[nhy == max(nhy)] # the GO ID(s) with the most direct children

  ## Some sanity check
  sum(nhy) == count.links(y) # TRUE
}

\keyword{methods}

