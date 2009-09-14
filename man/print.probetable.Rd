\name{print.probetable}
\alias{print.probetable}
\title{Print method for probetable objects}
\usage{
\method{print}{probetable}(x, \dots)
}
\description{
  Prints class(x), nrow(x) and ncol(x), but not the elements of x.
  The motivation for having this method is that methods from the package
  \code{base} such as
  \code{\link[base:print.dataframe]{print.data.frame}}
  will try to print the values of all elements of \code{x}, which can
  take inconveniently much time and screen space if \code{x} is large.
}
\arguments{
  \item{x}{an object of S3-class \code{probetable}.}
  \item{\dots}{further arguments that get ignored.}
}
\seealso{
  \code{\link[base:print.dataframe]{print.data.frame}}
}
\examples{
  a = as.data.frame(matrix(runif(1e6), ncol=1e3))
  class(a) = c("probetable", class(a))
  print(a)
  print(as.matrix(a[2:3, 4:6]))
}
\keyword{print}
