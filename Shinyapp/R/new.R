new <-
function (Class, ...) 
{
    ClassDef <- getClass(Class, where = topenv(parent.frame()))
    value <- .Call(C_new_object, ClassDef)
    initialize(value, ...)
}
