#' @title Constructor for "immutable" S3-class property
#'
#' @description
#' This method should allow any S3 object in R to become immutable by
#' intercepting `[<-`, `[[<-`, `$<-` and `c` during S3-method dispatch and
#' returning an error. Also intercepts `cbind` and `rbind`.
#'
#' Reverse with call to the `mutable` function.
#'
#' @details
#' The motivation for this class was to create pseudo-private slots in an R
#' S4 object by preventing mutation of those slots outside of the accessors
#' written for the class. It should behave as expected for R object which
#' operate with 'copy-on-modify' semantics, including most base R functions and
#' S3 objects.
#'
#' An environment was not suitable for this case due
#' to the copy-by-reference semantics, such that normal R assignment, which
#' users assume makes a copy of the object, actually references the same
#' environment in both the original and copy of the object.
#'
#' WARNING: This method has only been tested with `list` and `data.frame`.
#' Developers should check that it behaves as expected for your class of
#' interest. It is likely this will not work for `S4` object or objects which
#' have modify by reference semantics such as a `data.table` or `environment`.
#'
#' @param object Any R object
#'
#' @return The `object` with "immutable" prepended to its class attribute.
#'
#' @examples
#' immutable_list <- immutable(as.list(1:5))
#' class(immutable_list)
#' # errors during assignment operations
#' tryCatch({ immutable_list$new <- 1 }, error=print)
#'
#' @export
immutable <- function(object) {
    structure(object, class=c("immutable", class(object)))
}


#' @title Check if an R object inherits from the immutable S3-class.
#'
#' @param object Any R object to check if it inherits from the "immutable"
#' S3-class.
#'
#' @return `logical(1)` Does the object inherit from the "immutable" S3-class?
#'
#' @export
is.immutable <- function(object) {
    is(object, "immutable")
}


#' @title Print method for objects inheriting from the "immutable" S3-class
#'
#' @param x An object inheriting from the "immutable" S3-class.
#'
#' @return None, `invisible(NULL)`
#'
#' @md
#' @export
print.immutable <- function(x) {
    other_cls <- setdiff(class(x), "immutable")
    cat("Immutable class:", other_cls, "\n")
    class(x) <- other_cls
    print(x)
}


# -- Intercept subset and concatentate operations to return another "immutable"

#' @title Intercept concatenation for "immutable" class objects to return
#' another "immutable" class object.
#'
#' @description
#' Ensures that `c` and `append` to an "immutable" class object return an
#' immutable class object.
#'
#' @param x
#' @param ...
#'
#' @md
#' @export
c.immutable <- function(x, ...) {
    new_obj <- NextMethod()
    immutable(new_obj)
}


#' @title Subset an immutable object, returning another immutable object.
#'
#' @param x An R object inheriting from the "immutable" S3-class.
#' @param ... Catch any additional parameters. Lets objects with arbitrary
#' dimensions be made immutable.
#'
#' @md
#' @export
subset.immutable <- function(x, ...) {
    sub_obj <- NextMethod()
    immutable(sub_obj)
}


# -- Intercept assignment to prevent modification

#' @title
#'
#' @param object An R object inherting from the "immutable" S3-class.
#' @param ... Catch subset arguments for various dimensions.
#'
#' @aliases `[<-.immutable`, `[[<-.immutable`, `$<-.immutable`
#'
`[<-.immutable` <- `[[<-.immutable` <- `$<-.immutable` <- function(object, ..., value)
    stop("Object is immutable! Use `mutable(object)` to allow modification.",
        call.=FALSE)

# -- Remove immutability from an R object

#' @title Mutable S3-generic definition.
#'
#' @param object An R object.
#'
#' @return The `object`
#'
#' @md
#' @export
mutable <- function(object) UseMethod("mutable", object)
#'
#' @md
#' @export
mutable.default <- function(object) {
    structure(object, class=setdiff("immutable", class(object)))
}
