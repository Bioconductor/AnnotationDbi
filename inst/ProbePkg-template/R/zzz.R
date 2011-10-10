.onLoad <- function(libname, pkgname) {
  require("AnnotationDbi", quietly=TRUE)
  
  ## load the data
  where = asNamespace(pkgname)
  data(list = pkgname, package = pkgname, envir = where)
}
