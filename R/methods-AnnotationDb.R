AnnotationDb <-
    function(conn, pkg, ...)
{
    .AnnotationDb$new(conn=conn, package=pkg...)
}

setMethod(show, "AnnotationDb",
   function(object)
{
    cat("class:", class(object), "\n")
})
