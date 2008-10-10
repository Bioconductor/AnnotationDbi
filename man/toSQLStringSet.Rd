\name{toSQLStringSet}
\alias{toSQLStringSet}

\title{Convert a vector to a quoted string for use as a SQL value list}
\description{
  Given a vector, this function returns a string with each element of
  the input coerced to character, quoted, and separated by ",".
}
\usage{
toSQLStringSet(names)
}
\arguments{
  \item{names}{A vector of values to quote}
}
\details{
  If \code{names} is a character vector with elements containing single
  quotes, these quotes will be doubled so as to escape the quote in SQL.
}
\value{
  A character vector of length one that represents the input vector as a
  SQL value list.  Each element is single quoted and elements are
  comma separated.
}
\author{Herve Pages}
\note{
  Do not use \code{sQuote} for generating SQL as that function is
  intended for display purposes only.  In some locales, \code{sQuote}
  will generate fancy quotes which will break your SQL.
}
\examples{
toSQLStringSet(letters[1:4])
toSQLStringSet(c("'foo'", "ab'cd", "bar"))

}

\keyword{utilities}
\keyword{manip}
