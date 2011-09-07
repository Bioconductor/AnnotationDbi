AnnotationDb <-
    function(conn,  ...)
{
    .AnnotationDb$new(conn=conn,  ...)
}

## setMethod(show, "AnnotationDb",
##    function(object)
## {
##     cat("class:", class(object), "\n")
## })

## Try to make this generic
setMethod("show", "AnnotationDb",
    function(object)
    {
        cat(class(object)," object:\n")
        metadata <- metadata(object)
        for (i in seq_len(nrow(metadata))) {
            cat("| ", metadata[i, "name"], ": ", metadata[i, "value"],
                "\n", sep="")
        }
    }
)
