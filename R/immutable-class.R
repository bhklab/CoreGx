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
    if (isS4(object)) stop("Can only set immutability for base and S3 classes!")
    # call mutable to prevent assigning immutable class twice
    structure(mutable(object), class=c("immutable", attributes(object)$class))
}

# register the new S3 class, so it can be used in S4 method dispatch
#' @export
setOldClass("immutable")


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
    other_cls <- setdiff(attributes(x)$class, "immutable")
    class(x) <- other_cls
    cat("immutable", class(x), "\n")
    print(x)
}

#' @export
show.immutable <- function(x) print(x)


# -- Intercept subset and concatentate operations to return another "immutable"

#' @title Intercept concatenation for "immutable" class objects to return
#' another "immutable" class object.
#'
#' @description
#' Ensures that `c` and `append` to an "immutable" class object return an
#' immutable class object.
#'
#' @param x An R object inheriting from the "immutable" S3-clas
#' @param ... Objects to concatenate to `x`.
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
#' @name [.immutable
#' @rdname setOps-immutable
#' @export
`[.immutable` <- function(x, ...) {
    if (is.data.table(x)) {
        dots <- substitute(alist(...))
        j_expr <- dots[["j"]]
        if (is.null(j_expr)) j_expr <- dots[[2 + 1]]  # index plus one due to alist call
        j_txt <- deparse(j_expr)
        if (grepl(":=|let[ ]*\\(|set[ ]*\\(", j_txt))
            stop("This data.table is immutable! No assignment by reference ",
                "allowed. Use `mutable(x)` to return a mutable copy.",
                call.=FALSE)
    }
    sub_obj <- NextMethod()
    immutable(sub_obj)
}
#' @name [[.immutable
#' @rdname setOps-immutable
#' @export
`[[.immutable` <- function(x, ...) {
    sub_obj <- NextMethod()
    immutable(sub_obj)
}
#' @name $.immutable
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
#' @param object An R object inherting from the "immutable" S3-class.
#' @param ... Catch subset arguments for various dimensions.
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
#' @aliases subset<-.immutable, [<-.immutable, [[<-.immutable, $<-.immutable
#' @export
`subset<-.immutable` <- function(object, ..., value) {
    stop(.immutable_emsg, call.=FALSE)
}
#' @name [<-.immutable
#' @rdname assignment-immutable
#' @export
`[<-.immutable` <- function(object, ..., value) {
    stop(.immutable_emsg, call.=FALSE)
}
#' @name [[<-.immutable
#' @rdname assignment-immutable
#' @export
`[[<-.immutable` <- function(object, ..., value) {
    stop(.immutable_emsg, call.=FALSE)
}
#' @name $<-.immutable
#' @rdname assignment-immutable
#' @export
`$<-.immutable` <- function(object, ..., value) {
    stop(.immutable_emsg, call.=FALSE)
}

#' @rdname assignment-immutable
#' @aliases names<-.immutable, dimnames<-.immutable, colnames<-.immutable,
#' rownames<-.immutable
#' @export
`names<-.immutable` <- function(x, value)
    stop(.immutable_emsg, call.=FALSE)
#' @name dimnames<-.immutable
#' @rdname assignment-immutable
#' @export
`dimnames<-.immutable` <- function(x, value)
    stop(.immutable_emsg, call.=FALSE)
#' @name colnames<-.immutable
#' @rdname assignment-immutable
#' @export
`colnames<-.immutable` <- function(x, value)
    stop(.immutable_emsg, call.=FALSE)
#' @name rownames<-.immutable
#' @rdname assignment-immutable
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
#' @md
#' @export
mutable <- function(object) UseMethod("mutable", object)
#'
#' @md
#' @export
mutable.default <- function(object) {
    new_class <- setdiff(attributes(object)$class, "immutable")
    structure(object, class=new_class)
}


# -- Make comparisons work for immutable objects

#' @export
Ops.immutable <- function(e1, e2) {
    get(.Generic)(mutable(e1), mutable(e2))
}