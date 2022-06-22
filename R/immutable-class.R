#' @title Constructor for "immutable" S3-class property
#'
#' @description
#' This method should allow any S3 object in R to become immutable by
#' intercepting `[<-`, `[[<-`, `$<-` and `c` during S3-method dispatch and
#' returning an error.
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
#' to the 'copy-by-reference' semantics, such that normal R assignment, which
#' users assume makes a copy of the object, actually references the same
#' environment in both the original and copy of the object.
#'
#' WARNING: This implementation is unable to intercept modifications to a
#' `data.table` via the `set*` group of methods. This is because these methods
#' are not S3 generics and therefore no mechanism exists for hooking into them
#' to extend their functionality. In general, this helper class will only work
#' for objects with an S3 interface.
#'
#' @param object,x Any R object which uses S3 method dispatch
#'
#' @return The `object` with "immutable" prepended to its class attribute.
#'
#' @examples
#' immutable_list <- immutable(as.list(1:5))
#' class(immutable_list)
#' # errors during assignment operations
#' tryCatch({ immutable_list$new <- 1 }, error=print)
#'
#' @seealso
#' [`assignment-immutable`], [`setOps-immutable`]
#'
#' @md
#' @rdname immutable
#' @name immutable
#' @export
immutable <- function(object) {
    if (isS4(object)) stop("Can only set immutability for base and S3 classes!")
    # call mutable to prevent assigning immutable class twice
    structure(mutable(object), class=c("immutable", attributes(object)$class))
}

# register the new S3 class, so it can be used in S4 method dispatch
#' @rdname immutable
#' @name immutable
#' @export
setOldClass("immutable")

#' @rdname immutable
#' @export
setClassUnion("immutable_list", c("immutable", "list"))


#' @title Check if an R object inherits from the immutable S3-class.
#'
#' @return `logical(1)` Does the object inherit from the "immutable" S3-class?
#'
#' @examples
#' immutable_list <- immutable(as.list(1:5))
#' is.immutable(immutable_list)
#'
#' @rdname immutable
#' @export
is.immutable <- function(object) {
    is(object, "immutable")
}


#' @title Print method for objects inheriting from the "immutable" S3-class
#'
#' @param ... Fallthrough arguments to `print.default`.
#'
#' @return None, `invisible(NULL)`
#'
#' @rdname immutable
#' @md
#' @export
print.immutable <- function(x, ...) {
    other_cls <- setdiff(attributes(x)$class, "immutable")
    class(x) <- other_cls
    cat("immutable", class(x), "\n")
    print(x, ...)
}

#' @rdname immutable
#' @export
show.immutable <- function(x) print(x)


# -- Intercept subset and concatentate operations to return another "immutable"

#' @title Intercept concatenation for "immutable" class objects to return another "immutable" class object.
#'
#' @description
#' Ensures that `c` and `append` to an "immutable" class object return an
#' immutable class object.
#'
#' @param x An R object inheriting from the "immutable" S3-clas
#' @param ... Objects to concatenate to `x`.
#'
#' @return x with one or more values appended to it.
#'
#' @md
#' @export
c.immutable <- function(x, ...) {
    new_obj <- NextMethod()
    immutable(new_obj)
}

.immutable_emsg <- "Object is immutable! Use `mutable(object)` to return a mutable copy."

#' @name setOps-immutable
#' @rdname setOps-immutable
#'
#' @title Subset an immutable object, returning another immutable object.
#'
#' @param x An R object inheriting from the "immutable" S3-class.
#' @param ... Catch any additional parameters. Lets objects with arbitrary
#' dimensions be made immutable.
#'
#' @return An immutable subset of `x`.
#'
#' @examples
#' immut_mat <- immutable(matrix(1:100, 10, 10))
#' immut_mat[1:5, 1:5]
#'
#' @md
#' @aliases subset.immutable, [.immutable, [[.immutable, $.immutable
#' @export
subset.immutable <- function(x, ...) {
    sub_obj <- NextMethod()
    immutable(sub_obj)
}
#' @name [
#' @rdname setOps-immutable
#' @export
`[.immutable` <- function(x, ...) {
    if (is.data.table(x)) {
        dots <- substitute(alist(...))
        j_expr <- dots[["j"]]
        if (is.null(j_expr) && length(dots) > 2)
            j_expr <- dots[[2 + 1]]  # index plus one due to alist call
        if (!is.null(j_expr))
            j_txt <- deparse(j_expr)
            if (grepl(":=|let[ ]*\\(|set[ ]*\\(", j_txt))
                stop("This data.table is immutable! No assignment by reference ",
                "allowed. Use `mutable(x)` to return a mutable copy.",
                    call.=FALSE)
    }
    sub_obj <- NextMethod()
    immutable(sub_obj)
}
#' @name [[
#' @rdname setOps-immutable
#' @export
`[[.immutable` <- function(x, ...) {
    sub_obj <- NextMethod()
    immutable(sub_obj)
}
#' @name $
#' @rdname setOps-immutable
#' @export
`$.immutable` <- function(x, ...) {
    sub_obj <- NextMethod()
    immutable(sub_obj)
}

# -- Intercept assignment to prevent modification

#' @name assignment-immutable
#' @rdname assignment-immutable
#'
#' @title Intercept assignment operations for "immutable" S3 objects.
#'
#' @description
#' Prevents modification of objects labelled with the "immutable" S3-class by
#' intercepting assignment during S3-method dispatch and returning an error.
#'
#' @param object,x An R object inherting from the "immutable" S3-class.
#' @param ... Catch subset arguments for various dimensions.
#' @param value Not used.
#'
#' @return None, throws an error.
#'
#' @examples
#' immutable_df <- immutable(data.frame(a=1:5, b=letters[1:5]))
#' # return immutable data.frame
#' immutable_df[1:4, ]
#' # return immutable vector
#' immutable_df$a
#'
#' @md
#' @usage \\method{subset}{immutable}(object, ...) <- value
#' @aliases subset<-.immutable, [<-.immutable, [[<-.immutable, $<-.immubtale,
#' colnames<-.immutable, rownames<-.immutable, dimnames<-.immutable,
#' names<-.immutable
#' @export
`subset<-.immutable` <- function(object, ..., value) {
    stop(.immutable_emsg, call.=FALSE)
}
#' @name [<-
#' @rdname assignment-immutable
#' @export
`[<-.immutable` <- function(object, ..., value) {
    stop(.immutable_emsg, call.=FALSE)
}
#' @name [[<-
#' @rdname assignment-immutable
#' @export
`[[<-.immutable` <- function(object, ..., value) {
    stop(.immutable_emsg, call.=FALSE)
}
#' @name $<-
#' @rdname assignment-immutable
#' @export
`$<-.immutable` <- function(object, ..., value) {
    stop(.immutable_emsg, call.=FALSE)
}
#' @name names<-
#' @rdname assignment-immutable
#' @export
`names<-.immutable` <- function(x, value)
    stop(.immutable_emsg, call.=FALSE)
#' @name dimnames<-
#' @rdname assignment-immutable
#' @export
`dimnames<-.immutable` <- function(x, value)
    stop(.immutable_emsg, call.=FALSE)
#' @name colnames<-
#' @rdname assignment-immutable
#' @usage \\method{colnames}{immutable}(x) <- value
#' @export
`colnames<-.immutable` <- function(x, value)
    stop(.immutable_emsg, call.=FALSE)
#' @name rownames<-
#' @rdname assignment-immutable
#' @usage \\method{rownames}{immutable}(x) <- value
#' @export
`rownames<-.immutable` <- function(x, value)
    stop(.immutable_emsg, call.=FALSE)


# -- Remove immutability from an R object


#' @title Remove the "immutable" S3-class from an R object, allowing it to be
#' modified normally again.
#'
#' @param object An R object inheriting from the "immutable" class.
#'
#' @return The `object` with the "immutable" class stripped from it.
#'
#' @examples
#' immut_list <- immutable(list())
#' mutable(immut_list)
#'
#' @md
#' @export
mutable <- function(object) UseMethod("mutable", object)
#'
#' @md
#' @importFrom data.table copy
#' @export
mutable.default <- function(object) {
    new_class <- setdiff(attributes(object)$class, "immutable")
    structure(copy(object), class=new_class)
}


# -- Make comparisons work for immutable objects

#' @export
Ops.immutable <- function(e1, e2) {
    get(.Generic)(mutable(e1), mutable(e2))
}