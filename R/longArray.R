#require(S4Vectors)
#require(SummarizedExperiment)


#' @import S4Vectors
#' @import SummarizedExperiment
#' @import data.table
setOldClass("data.table")
setClassUnion("dt.df", c("data.table", "data.frame"))

.longArray <- setClass("longArray", slots = list(col.ids = "dt.df",
                                                 row.ids = "dt.df",
                                                 listData = "list",
                                                 storage.type = "character"),
                                                 contains = c("List"))


.longArray.DT <- setClass("longArray.DT", contains = "longArray")


#' construct a longArray instance
#' @param row.ids define
#' @param col.ids define
#' @param data define
#' @param \dots define
#' @export
longArray <- function(row.ids, col.ids, data, ...){
    dot.args <- list(...)

    all(as.data.frame(col.ids)[,1] == as.data.frame(row.ids)[,1]) || stop("Column and Row defining tables have different number of PKs")

    if(is(row.ids[[1]], "factor")){
        row.ids[[1]] <- as.character(row.ids[[1]])
    }


    if(is(col.ids[[1]], "factor")){
        col.ids[[1]] <- as.character(col.ids[[1]])
    }
    data <- lapply(data, function(x){
        x[[1]] <- as.character(x[[1]])
        return(x)
    })

    if("data.table" %in% class(row.ids)){
        setkey(row.ids)
        
    }
    if("data.table" %in% class(col.ids)){
        setkey(col.ids)   
    }



    colnames(row.ids)[1] <- "id"
    colnames(col.ids)[1] <- "id"
    # if(!"list" %in% class(data)){
    #     data <- list(data)
    # }
    if("data.table" %in% class(data[[1]])){
        storage.type <- "data.table"

        ## I am using the wrong idiom below::
        data <- lapply(data, function(xx) {
            colnames(xx)[1] <- "id"; return(xx)
        })
        if(!all(sapply(data, function(xx) xx[,all(id %in% row.ids[,id])]))) stop("Error")
        for(dd in data){
            setkeyv(dd, "id")
        }
        return(.longArray.DT(col.ids = col.ids, row.ids = row.ids, listData = data, storage.type = storage.type))
    } else {
        storage.type <- "data.frame"
        stop("Not Implemented")
    }
}

setMethod(`[`, "longArray.DT", function(x, i, j, ..., drop=if (missing(i)) TRUE else length(cols) == 1){
    ## how to do this efficiently? From tests, it seems that %in% is the fastest?
    ## Faster than "on = " ??? Seems not after more testing
    mdrop <- missing(drop)
    Narg <- nargs() - !mdrop
    if(missing(i)){
        i <- rownames(x)
    }
    if(missing(j)){
        if(Narg == 2) {
            j <- i
            i <- rownames(x)
        } else {
            j <- colnames(x)
        }
    }

    if(is.null(j)){
        return(NULL)
    }

    if(is.null(i)){
        return(NULL)
    }

    if(is.numeric(i)){
        warning("Numeric i index was passed, indexing into rownames")
        i <- rownames(x)[i]
    }
    if(is.numeric(j)){
        warning("Numeric j index was passed, indexing into colnames")
        j <- colnames(x)[j]
    }
    if(is.logical(i)){
        i <- rownames(x)[i]
    }
    if(is.logical(j)){
        j <- colnames(x)[j]
    }

    if(anyDuplicated(i)){
        warning("Duplicate row ids were passed to object, taking unique ids")
        i <- unique(i)
    } 
    if(anyDuplicated(j)){
        warning("Duplicate column ids were passed to object, taking unique ids")
        j <- unique(j)
    }

    row.ids <- x@row.ids
    iCols <- colnames(x@row.ids)[-1]
    for (iCol in iCols){
        row.ids <- row.ids[list(i), on=iCol, nomatch=0]
    }
    rowIds <- row.ids[,id]


    col.ids <- x@col.ids[rowIds,]
    jCols <- colnames(x@col.ids)[-1]
    for (jCol in jCols){
        col.ids <- col.ids[list(j), on=jCol, nomatch=0]
    }

    ids <- col.ids[,id]

    newXData <- lapply(x@listData, function(x) return(x[ids,]))
    return(longArray(col.ids = col.ids[ids,on="id"], row.ids = row.ids[ids,on="id"], data = newXData))
})


## XXX: Breaks when listData[[i]] has no columns except PK!!!
setMethod("[[", c("longArray", "ANY", "missing"),
    function(x, i, j, ...)
{   
    if(! i %in% names(x)) return(NULL)
    ids <- x@listData[[i]][,id]
    return(cbind(x@row.ids[.(ids),],x@col.ids[.(ids),-1],x@listData[[i]][,-1]))
})

setReplaceMethod("[[", c("longArray.DT", "ANY", "missing"),
    function(x, i, j, ..., value)
{
    value <- data.table(value)
    if(!all(value[[1]] %in% getkeys(x))){
        stop("First column of the replacement must contain primary keys and match into the currently defined primary keys in the ")
    }
    colnames(value)[[1]] <- "id"
    setkeyv(value, "id")
    x@listData[[i]] <- value
    x
})

.DollarNames.longArray <- function(x, pattern = "")
    grep(pattern, names(x@listData), value=TRUE)

setMethod("$", "longArray",
    function(x, name)
{
    x[[name]]
})

# setReplaceMethod("$", "SummarizedExperiment",
#     function(x, name, value)
# {
#     colData(x)[[name]] <- value
#     x
# })



# setGeneric("toTable", function(x) standardGeneric("toTable"))
# setMethod("toTable", "longArray.DT", function(x){
#     # return(x@data[x@info, on = colnames(x@data)[1]])
#     return(x@info[x@data, on = colnames(x@data)[1]])

# }) 

## TODO:: FIXME:: this is inefficient 
setMethod("dim", "longArray.DT", function(x){
    return(c(length(unique(unlist(x@row.ids[, -1]))), 
             length(unique(unlist(x@col.ids[, -1])))))
})


setMethod("dimnames", "longArray.DT", function(x){
    return(list(unique(unlist(x@row.ids[, -1])), 
                unique(unlist(x@col.ids[, -1]))))
})

setMethod("show", signature=signature(object="longArray"), 
    function(object) {
        dms <- dim(object)
        cat("A longArray object with the following dimensions: \n")
        cat(dms[1],"\"rows\" across ", length(object@row.ids) - 1," variable(s) \n")
        cat(dms[2],"\"columns\" across ", length(object@col.ids) - 1," variable(s) \n")
})

setMethod("names", "longArray", function(x) return(names(x@listData)))
setMethod("length", "longArray", function(x) return(length(x@listData)))

setGeneric("getkeys", function(x) standardGeneric("getkeys"))
setMethod("getkeys", "longArray", function(x) return(unlist(x@col.ids[[1]])))

setGeneric("getIDColNames", function(x) standardGeneric("getIDColNames"))
setMethod("getIDColNames", "longArray", function(x) return(union(colnames(x@col.ids), colnames(x@row.ids))))
# sens.info[rep(list(rCI), 2), on=c("cellid", "drugid"), nomatch=0]
