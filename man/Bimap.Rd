\name{Bimap}
\docType{class}

\alias{Bimap}

% Classes

\alias{Bimap}
\alias{class:Bimap}
\alias{Bimap-class}

\alias{AnnDbBimap}
\alias{class:AnnDbBimap}
\alias{AnnDbBimap-class}

\alias{GoAnnDbBimap}
\alias{class:GoAnnDbBimap}
\alias{GoAnnDbBimap-class}

\alias{Go3AnnDbBimap}
\alias{class:Go3AnnDbBimap}
\alias{Go3AnnDbBimap-class}

\alias{GOTermsAnnDbBimap}
\alias{class:GOTermsAnnDbBimap}
\alias{GOTermsAnnDbBimap-class}

\alias{AnnDbMap}
\alias{class:AnnDbMap}
\alias{AnnDbMap-class}

\alias{IpiAnnDbMap}
\alias{class:IpiAnnDbMap}
\alias{IpiAnnDbMap-class}

\alias{AgiAnnDbMap}
\alias{class:AgiAnnDbMap}
\alias{AgiAnnDbMap-class}

% Standard generic methods

\alias{show,FlatBimap-method}
\alias{show,AnnDbBimap-method}

\alias{summary,Bimap-method}
\alias{summary,AnnDbBimap-method}


\title{Bimap objects and the Bimap interface}

\description{
  What we usually call "annotation maps" are in fact Bimap objects.
  In the following sections we present the bimap concept and the
  Bimap interface as it is defined in AnnotationDbi.
}

\section{Display methods}{
  In the code snippets below,
  \code{x} is a Bimap object.

  \describe{
    \item{}{
      \code{show(x)}:
      Display minimal information about Bimap object \code{x}.
    }

    \item{}{
      \code{summary(x)}:
      Display a little bit more information about Bimap object \code{x}.
    }
  }
}

\section{The bimap concept}{
  A bimap is made of:
  \preformatted{
  - 2 sets of objects: the left objects and the right objects.
    All the objects have a name and this name is unique in
    each set (i.e. in the left set and in the right set).
    The names of the left (resp. right) objects are called the
    left (resp. right) keys or the Lkeys (resp. the Rkeys).
  }
  \preformatted{   
  - Any number of links (edges) between the left and right
    objects. Note that the links can be tagged. In our model,
    for a given bimap, either none or all the links are tagged.
  }

  In other words, a bimap is a bipartite graph.

  Here are some examples:
  \preformatted{
  1. bimap B1:

     4 left objects (Lkeys): "a", "b", "c", "d"
     3 objects on the right (Rkeys): "A", "B", "C"

     Links (edges):
      "a" <--> "A"
      "a" <--> "B"
      "b" <--> "A"
      "d" <--> "C"

     Note that:
       - There can be any number of links starting from or ending
         at a given object.
       - The links in this example are untagged.
  }
  \preformatted{
  2. bimap B2:

     4 left objects (Lkeys): "a", "b", "c", "d"
     3 objects on the right (Rkeys): "A", "B", "C"

     Tagged links (edges):
       "a" <-"x"-> "A"
       "a" <-"y"-> "B"
       "b" <-"x"-> "A"
       "d" <-"x"-> "C"
       "d" <-"y"-> "C"

     Note that there are 2 links between objects "d" and "C":
     1 with tag "x" and 1 with tag "y".
  }
}

\section{Flat representation of a bimap}{
  The flat representation of a bimap is a data frame. For
  example, for B1, it is:
  \preformatted{
    left  right
       a      A 
       a      B
       b      A
       d      C
  }
  If in addition the right objects have 1 multivalued attribute,
  for example, a numeric vector:
  \preformatted{
    A <-- c(1.2, 0.9)
    B <-- character(0)
    C <-- -1:1
  }
  then the flat representation of B1 becomes:
  \preformatted{
    left  right  Rattrib1
       a      A       1.2
       a      A       0.9
       a      B        NA
       b      A       1.2
       b      A       0.9
       d      C        -1
       d      C         0
       d      C         1
  }
  Note that now the number of rows is greater than the number
  of links!
}

\section{AnnDbBimap and FlatBimap objects}{
  An AnnDbBimap object is a bimap whose data are stored in a data base.
  A FlatBimap object is a bimap whose data (left keys, right keys and
  links) are stored in memory (in a data frame for the links).
  Conceptually, AnnDbBimap and FlatBimap objects are the same (only
  their internal representation differ) so it's natural to try to define
  a set of methods that make sense for both (so they can be manipulated
  in a similar way). This common interface is the Bimap interface.

  Note that both AnnDbBimap and FlatBimap objects have a read-only
  semantic: the user can subset them but cannot change their data.
}

\section{The "flatten" generic}{
  \preformatted{
    flatten(x) converts AnnDbBimap object x into FlatBimap
    object y with no loss of information
  }
  Note that a FlatBimap object can't be converted into an AnnDbBimap
  object (well, in theory maybe it could be, but for now the data bases
  we use to store the data of the AnnDbBimap objects are treated as
  read-only). This conversion from AnnDbBimap to FlatBimap is performed
  by the "flatten" generic function (with methods for AnnDbBimap objects
  only). 
}

\section{Property0}{
  The "flatten" generic plays a very useful role when we need to
  understand or explain exactly what a given Bimap method f will do when
  applied to an AnnDbBimap object. It's generally easier to explain what
  it does on a FlatBimap object and then to just say "and it does the
  same thing on an AnnDbBimap object". This is exactly what Property0
  says:
  \preformatted{
    for any AnnDbBimap object x, f(x) is expected to be
    indentical to f(flatten(x))
  }
  Of course, this implies that the f method for AnnDbBimap objects
  return the same type of object than the f method for FlatBimap objects.
  In this sense, the "revmap" and "subset" Bimap methods are particular
  because they are expected to return an object of the same class as
  their argument x, so f(x) can't be identical to f(flatten(x)). For
  these methods, Property0 says:
  \preformatted{
    for any AnnDbBimap object x, flatten(f(x)) is expected to
    be identical to f(flatten(x))
  }

  Note to the AnnotationDbi maintainers/developpers: the \code{checkProperty0}
  function (AnnDbPkg-checker.R file) checks that Property0 is satisfied on all
  the AnnDbBimap objects defined in a given package (FIXME: checkProperty0 is
  currently broken).
}

\section{The Bimap interface in AnnotationDbi}{
  The full documentation for the methods of the Bimap interface is splitted
  into 4 man pages: \link{Bimap-direction}, \link{Bimap-keys}
  and \link{Bimap-toTable}.
}

\seealso{
  \link{Bimap-direction},
  \link{Bimap-keys},
  \link{Bimap-toTable},
  \link{BimapFormatting},
  \link{AnnDbBimap-envirAPI}
}

\examples{
  library(hgu95av2.db)
  ls(2)
  hgu95av2GO # calls the "show" method
  summary(hgu95av2GO)
  hgu95av2GO2PROBE # calls the "show" method
  summary(hgu95av2GO2PROBE)
}

\keyword{classes}
\keyword{interface}
