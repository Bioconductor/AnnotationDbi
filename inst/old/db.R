## Most of this file belongs in an AnnotationDBI or similarly named package.
## However, the first bits that reference the 'globals' env probably need
## to be generated and part of the actual data package.  The exception is
## globals$DEBUG which really belongs to the interface layer.

## We use a package-global environment to store table names, DB path,
## and the connection to the DB itself.  Using an env is key because it
## allows us to establish the DB con inside .onLoad via the
## initDbConnection() function.
globals <- new.env(parent=emptyenv())
globals$DEBUG <- FALSE

toggleDebug <- function() {
    globals$DEBUG <- !(globals$DEBUG)
}

validateFields <- function(fields, theFields) {
    ## Raise an error if fields are not columns of table.
    badFields <- fields[!(fields %in% theFields)]
    if (length(badFields) > 0)
      stop("unknown field names:\n", paste(badFields, collapse=", "),
           "\nknown fields are:\n", paste(theFields, collapse=", "))
}

