#' @include DataMapper-class.R
NULL

#' A Class for Mapping Between Raw Data and an `LongTable` Object
#'
#' @slot rawdata See Slots section.
#' @slot rowDataMap See Slots section.
#' @slot colDataMap See Slots section.
#' @slot assayMap See Slots section.
#' @slot metadataMap See Slots section.
#'
#' @inheritSection DataMapper-class Slots
#'
#' @section Slots:
#' * rowDataMap: A list-like object containing two `character` vectors.
#' The first is column names in `rawdata` needed to uniquely identify each row,
#' the second is additional columns which map to rows, but are not required to
#' uniquely identify them. Rows should be drugs.
#' * colDataMap: A list-like object containing two `character` vectors.
#' The first is column names in `rawdata` needed to uniquely identify each
#' column, the second is additional columns which map to rows, but are not
#' required to uniquely identify them. Columns should be samples.
#' * assayMap A list-like where each item is a `list` with two elements
#' specifying an assay, the first being the identifier columns in `rawdata`
#' needed to uniquely identify each row an assay, and the second a list of
#' `rawdata` columns to be mapped to that assay. The names of `assayMap`
#' will be the names of the assays in the `LongTable` that is created when
#' calling `metaConstruct` on this `DataMapper` object.
#' * metadataMap: A list-like where each item is a `character` vector of
#' `rawdata` column names to assign to the `@metadata` of the `LongTable`,
#' where the name of that assay is the name of the list item. If names are
#' omitted, assays will be numbered by their index in the list.
#'
#' @md
#' @aliases LongTableDataMapper-class
.LongTableDataMapper <- setClass('LongTableDataMapper',
    contains=c('DataMapper'),
    slots=list(
        rowDataMap='list_OR_List',
        colDataMap='list_OR_List',
        assayMap='list_OR_List',
        metadataMap='list_OR_List'
    )
)


#' Constructor for the `LongTableDataMapper` class, which maps from one or
#'   more raw experimental data files to the slots of a `LongTable` object.
#'
#' @details
#' The `guessMapping` method can be used to test hypotheses about the
#' cardinality of one or more sets of identifier columns. This is helpful
#' to determine the id columns for `rowDataMap` and `colDataMap`, as well
#' as identify columns mapping to `assays` or `metadata`.
#'
#' To attach metadata not associated with `rawdata`, please use the `metadata`
#' assignment method on your `LongTableDataMapper`. This metadata will be
#' merged with any metadata from `metadataMap` and added to the `LongTable`
#' which this object ultimately constructs.
#'
#' @param rawdata A `data.frame` of raw data from a treatment response
#' experiment. This will be coerced to a `data.table` internally. We recommend
#' using joins to aggregate your raw data if it is not present in a single file.
#' @param rowDataMap A list-like object containing two `character` vectors.
#' The first is column names in `rawdata` needed to uniquely identify each row,
#' the second is additional columns which map to rows, but are not required to
#' uniquely identify them. Rows should be drugs.
#' @param colDataMap A list-like object containing two `character` vectors.
#' The first is column names in `rawdata` needed to uniquely identify each
#' column, the second is additional columns which map to rows, but are not
#' required to uniquely identify them. Columns should be samples.
#' @param assayMap A list-like where each item is a `list` with two `character`
#' vectors defining an assay, the first containing the identifier columns in
#' `rawdata` needed to uniquely identify each row an assay, and the second the
#' `rawdata` columns to be mapped to that assay. The names of `assayMap`
#' will be the names of the assays in the `LongTable` that is created when
#' calling `metaConstruct` on this `DataMapper` object. If the character vectors
#' have names, the value columns will be renamed accordingly.
#' @param metadataMap A list-like where each item is a `character` vector of
#' `rawdata` column names to assign to the `@metadata` of the `LongTable`,
#' where the name of that assay is the name of the list item. If names are
#' omitted, assays will be numbered by their index in the list.
#'
#' @return A `LongTable` object, with columns mapped to it's slots according
#' to the various maps in the `LongTableDataMapper` object.
#'
#' @seealso [`guessMapping`]
#'
#' @examples
#' data(exampleDataMapper)
#' exampleDataMapper
#'
#' @md
#' @importFrom checkmate assertList assertDataTable
#' @importFrom data.table setDT
#' @export
LongTableDataMapper <- function(rawdata=data.frame(),
        rowDataMap=list(character(), character()),
        colDataMap=list(character(), character()),
        assayMap=list(list(character(), character())),
        metadataMap=list(character())) {

    if (is(rawdata, 'data.frame') && !is(rawdata, 'data.table')) setDT(rawdata)
    assertDataTable(rawdata)
    assertList(rowDataMap, types="character", len=2)
    assertList(colDataMap, types="character", len=2)
    assertList(assayMap, types="list", min.len=1)
    for (i in seq_along(assayMap)) assertList(assayMap[[i]], types="character",
        len=2)
    assertList(metadataMap)

    .LongTableDataMapper(rawdata=rawdata, rowDataMap=rowDataMap,
        colDataMap=colDataMap, assayMap=assayMap, metadataMap=metadataMap)
}

## FIXME:: Modify rawdata setter to check that columns exist in maps for case
##>when maps are assigned first, then rawdata

# ======================
# DataMapper Show Method
# ----------------------


#' @describeIn LongTableDataMapper-class Show method for LongTableDataMapper.
#' Determines how the object is displayed in the console.
#'
#' @param object A `LongTableDataMapper` to display in the console.
#'
#' @examples
#' show(exampleDataMapper)
#'
#' @return `invisible` Prints to console.
#'
#' @importFrom crayon %+% yellow red green blue cyan magenta
#' @exportMethod show
setMethod('show', signature(object='LongTableDataMapper'), function(object) {

    ## -- class
    cat(yellow$bold$italic(paste0('<', class(object)[1], '>'), '\n'))

    missingVal <- ' NA'

    ## -- rawdata
    cat(yellow$bold('rawdata:'))
    if (length(rawdata(object))) {
        cat(paste0(' dim(', paste0(dim(rawdata(object)), collapse=', '), ')\n'))
        table_data <- capture.output(
            print(head(rawdata(object), 3), trunc.cols=TRUE, class=TRUE)
        )
        table_data[1] <- paste0('  ', table_data[1])
        rawdata_head <- paste0(
            paste0(table_data[-length(table_data)], collapse='\n  '),
            paste0(
                strwrap(table_data[length(table_data)], initial='\n  ', exdent=4),
                collapse='\n'
            ))
        cat(rawdata_head, '\n\r')  # print the snapshot
    } else {
        red(cat(missingVal, '\n'))
    }

    ## -- rowDataMap
    cat(yellow$bold('rowDataMap:'))
    rows <- rowDataMap(object)
    if (length(rows)) {
        cat('\n ', green('rowIDs:'), paste0(rows[[1]], collapse=', '))
    } else {
        cat(green(missingVal))
    }
    if (length(rows) > 1) {
        cat('\n ', green('rowMeta:'), paste0(rows[[2]], collapse=', '))
        cat('\n')
    } else {
        cat('\n')
    }

    ## -- colDataMap
    cat(yellow$bold('colDataMap:'))
    cols <- colDataMap(object)
    if (length(cols)) {
        cat('\n ', green('colIDs:'), paste0(cols[[1]], collapse=', '))
    } else {
        cat(green(missingVal))
    }
    if (length(cols) > 1) {
        cat('\n ', green('colMeta:'), paste0(cols[[2]], collapse=', '))
    }

    ## -- assayMap
    cat(yellow$bold('\nassayMap:'))
    assayM <- assayMap(object)
    if (length(assayM)) {
        for (aName in names(assayM)) {
            cat('\n ', red(paste0(aName, ':')))
            cat('\n    keys:',
                strwrap(paste0(assayM[[aName]][[1]], collapse=', '), exdent=2)
            )
            cat('\n    values:',
                strwrap(paste0(assayM[[aName]][[2]], collapse=', '), exdent=2)
            )
        }
    } else {
        cat(green(missingVal))
    }

    ## -- metadataMap
    cat(yellow$bold('\nmetadataMap:'))
    metadataM <- metadataMap(object)
    if (length(metadataM)) {
        for (mName in names(metadataM))
            cat('\n ', green(paste0(mName, ':')),
                paste0(metadataM[[mName]], collapse=', '))
    } else {
        cat(green(missingVal))
    }
    cat('\n')
})

## ===========================================
## LongTableDataMapper Accessors Documentation
## -------------------------------------------

.local_class_3 <- 'LongTableDataMapper'
.local_data_3 <- 'exampleDataMapper'

#' @name LongTableDataMapper-accessors
#'
#' @eval .docs_DataMapper_accessors(class_=.local_class_3)
#' @eval .docs_DataMapper_get_rawdata(class_=.local_class_3)
#'
#' @param value See details.
NULL


## =====================================
## LongTableDataMapper Accessors Methods
## -------------------------------------


## ---------------------
## ---- all slot helpers

#' Method to subset the rawdata with the corresponding dimensions "dimDataMap"
#'   method.
#'
#' @param x `LongTableDataMapper` or inheriting class.
#' @param key `logical(1)` Should the returned value be keyed by the 'id_columns'
#'   item of the dimDataMap? Ignored when dim is "meta".
#' @param dim `character(1)` Which dimension should rawdata to subset for? Options
#'   are "row", "col" and "meta", corresponding to the associated
#'   slots of the `LongTableDataMapper`.
#'
#' @importFrom checkmate assertClass assertLogical
#' @importFrom methods getPackageName
#' @keywords internal
.get_dimData <- function(x, key, dim=c("row", "col", "meta")) {

    # Input validation
    assertClass(x, "LongTableDataMapper")
    assertLogical(key)

    # Determine which slot and accessor function to use
    dim <- match.arg(dim)
    dimSlot <- paste0(dim, "DataMap")
    dimFun <- get(dimSlot)

    # Get method name to simplify debugging from S4 classes
    funContext <- paste0("\n[", getPackageName(), "::", dimSlot, ",",
        class(x)[1], "-method\n\t")

    .dimDataMap <- dimFun(x)

    # Ensure required data is present
    if (length(unlist(.dimDataMap)) < 1) stop(.errorMsg(funContext,
        "The ", dimSlot, " slot must contain valid data!"))

    return(.get_dimDataFromMap(x=x, key=(key && dim != "meta"),
        dataMap=.dimDataMap, funContext=funContext))
}

#' Method to subset the rawdata with the corresponding dimensions "dimDataMap"
#'   method.
#'
#' @param x `LongTableDataMapper` or inheriting class.
#' @param key `logical(1)` Should the returned value be keyed by the 'id_columns'
#'   item of the dimDataMap? Default is `TRUE`.
#' @param dataMap `list` The map of a `LongTableDataMapper` dimension, as
#'   returned by the '*DataMap' methods.
#' @param funContext `character(1)` Contextual information about the calling
#'   function, for debugging. Users don't need to worry about this.
#'
#' @importFrom checkmate assertClass assertLogical assertList
#' @importFrom methods getPackageName
#' @keywords internal
.get_dimDataFromMap <- function(x, key=TRUE, dataMap, funContext) {

    if (missing(funContext))
        funContext <- paste0("\n[", getPackageName(), "::.get_dimDataMap]\n\t")

    # Input validation
    assertClass(x, "LongTableDataMapper")
    assertLogical(key)
    assertList(dataMap, types=c("character"), max.len=2)

    # Extract relevant data
    .rawdata <- rawdata(x)

    # Ensure required data is present
    if (length(.rawdata) < 1) .error(funContext,
        "The rawdata slot must contain valid data!")
    hasDimDataCols <- unlist(dataMap) %in% colnames(.rawdata)
    if (!all(hasDimDataCols)) .error(funContext, "Columns ",
        .collapse(unlist(dataMap)[!hasDimDataCols]),
        " are missing from rawdata!")

    # Subset rawdata and return
    .dimData <- unique(.rawdata[, unlist(dataMap), with=FALSE])
    if (key) setkeyv(.dimData, dataMap$id_columns)
    return(.dimData)
}


## ---------------
## -- rawdata slot

#' @rdname LongTableDataMapper-accessors
#' @eval .docs_DataMapper_set_rawdata(class_=.local_class_3,
#' class1_='list')
setReplaceMethod('rawdata', signature=c(object='LongTableDataMapper',
        value='list'), function(object, value) {
    funContext <- .S4MethodContext('rawdata<-', class(object)[1],
        class(value)[1])

    rows <- unlist(rowDataMap(object))
    cols <- unlist(colDataMap(object))
    assays <- unlist(assayMap(object))
    meta <- unlist(metadataMap(object))

    ## TODO:: Improve parsing here such that it only throws warnings if meta-
    ##>data columns are missing

    mandatory <- c(rows, cols, assays, meta)
    if (!length(mandatory) || !length(value)) {
        object@rawdata <- value
    } else if (length(mandatory) && !length(rawdata(object))) {
        hasMandatory <- mandatory %in% colnames(value)
        if (!all(hasMandatory)) {
            stop(.errorMsg(funContext, "One or more map column is missing from value",
                ": ", paste0(mandatory[!hasMandatory], collapse=', '), '!'))
        }
        object@rawdata <- value
    } else {
        stop(.errorMsg(funContext, "In order to assign to the rawdata slot of ",
            "a LongTableDataMapper, either all the map slots must be ",
            "empty or the rawdata slot must be an empty list!"))
    }
    return(object)
})

## --------------------
## ---- rowDataMap slot


##
## -- rowDataMap

.docs_LongTableDataMapper_get_dimDataMap <- function(...) .parseToRoxygen(
    "
    @details
    __{dim_}DataMap__: `list` of two `character` vectors, the first are the
    columns required to uniquely identify each row of a `{class_}` and the
    second any additional {dim_}-level metadata. If the character vectors
    have names, the resulting columns are automatically renamed to the
    item name of the specified column.

    @examples
    {dim_}DataMap({data_})

    @md
    @aliases {dim_}DataMap,{class_}-method {dim_}DataMap
    @exportMethod {dim_}DataMap
    ",
    ...
)

#' @export
setGeneric('rowDataMap', function(object, ...) standardGeneric('rowDataMap'))

#' @rdname LongTableDataMapper-accessors
#' @eval
#' .docs_LongTableDataMapper_get_dimDataMap(dim_='row', class_=.local_class_3,
#' data_=.local_data_3)
setMethod('rowDataMap', signature(object='LongTableDataMapper'),
        function(object) {
    object@rowDataMap
})

#' @export
setGeneric('rowDataMap<-', function(object, ..., value)
    standardGeneric('rowDataMap<-'))

.docs_LongTableDataMapper_set_dimDataMap <- function(...) .parseToRoxygen(
    "
    @details
    __{dim_}DataMap<-__: Update the `@{dim_}DataMap` slot of a `{class_}` object,
    returning an invisible NULL. Arguments:
    - value: A `list` or `List` where the first item is the names of the
    identifier columns -- columns needed to uniquely identify each row in
    {dim_}Data -- and the second item is the metadata associated with those
    the identifier columns, but not required to uniquely identify rows in
    the object rowData.

    @examples
    {dim_}DataMap({data_}) <- list(c('{id_col_}'), c())

    @md
    @aliases rowDataMap<-,{class_},list-method {dim_}DataMap<-{class_},List-method {dim_}DataMap<-
    @exportMethod {dim_}DataMap<-
    ",
    ...
)


#' @rdname LongTableDataMapper-accessors
#' @eval
#' .docs_LongTableDataMapper_set_dimDataMap(dim_='row', class_=.local_class_3,
#' data_=.local_data_3, id_col_='drug_id')
setReplaceMethod('rowDataMap', signature(object='LongTableDataMapper',
        value='list_OR_List'), function(object, value) {
    funContext <- '[CoreGx::`rowDataMap<-`,LongTableDataMapper-method]\n\t'
    rawdataCols <- colnames(rawdata(object))

    # -- Handle error conditions
    if (length(value) > 2) {
        stop(.errorMsg(funContext, 'Assignments to rowDataMap should be a list ',
            'of length 2, where the first item is the name of the id columns ',
            'and the second item is the name of the metadata columns which ',
            'map to those id columns.'))
    }

    hasIDcols <- value[[1]] %in% rawdataCols
    if (!all(hasIDcols) && length(rawdata(object))) {
        stop(.errorMsg(funContext, 'One or more of the id columns specified ',
            'in value[[1]] are not valid column names in the rawdata slot of ',
            'this ', class(object)[1], ' object!'))
    }

    if (length(value) > 1 && length(value[[2]]) != 0 && length(rawdata(object))) {
        hasMetaCols <- value[[2]] %in% rawdataCols
        if (!all(hasMetaCols)) {
            stop(.errorMsg(funContext, 'The follow metadata columns in value[[2]] ',
                'are not present in rawdata(object): ',
                .collapse(value[[2]][!hasMetaCols]), '!'))
        }
        hasOneToOneRelationship <-
            value[[2]] %in% cardinality(rawdata(object), group=value[[1]])
        if (!all(hasOneToOneRelationship)) {
            stop(.errorMsg(funContext, 'The columns ',
                .collapse(value[[2]][!hasOneToOneRelationship], ' do not have a ',
                '1:1 relationship with the specified ID columns!')))
        }
    }

    # -- Function body
    object@rowDataMap <- value
    return(object)
})


##
## -- rowData


#' Convenience method to subset the `rowData` out of the `rawdata` slot using
#'   the assigned `rowDataMap` metadata.
#'
#' @param x `LongTableDataMapper` object with valid data in the `rawdata` and
#'   `colDataMap` slots.
#' @param key `logical(1)` Should the table be keyed according to the
#'   `id_columns` of the `rowDataMap` slot? This will sort the table in memory.
#'   Default is TRUE.
#'
#' @return `data.table` The `rowData` as specified in the `rowDataMap` slot.
#'
#' @export
setMethod("rowData", signature("LongTableDataMapper"), function(x, key=TRUE) {
    .get_dimData(x, key, dim="row")
})


## --------------------
## ---- colDataMap slot


##
## -- colDataMap

#' @export
setGeneric('colDataMap', function(object, ...) standardGeneric('colDataMap'))

#' @rdname LongTableDataMapper-accessors
#' @eval
#' .docs_LongTableDataMapper_get_dimDataMap(dim_='col', class_=.local_class_3,
#' data_=.local_data_3)
setMethod('colDataMap', signature(object='LongTableDataMapper'),
        function(object) {
    object@colDataMap
})

#' @export
setGeneric('colDataMap<-', function(object, ..., value) standardGeneric('colDataMap<-'))

#' @rdname LongTableDataMapper-accessors
#' @eval
#' .docs_LongTableDataMapper_set_dimDataMap(dim_='col', class_=.local_class_3,
#' data_=.local_data_3, id_col_='cell_id')
setReplaceMethod('colDataMap',
        signature(object='LongTableDataMapper', value='list_OR_List'),
        function(object, value) {
    funContext <- '[CoreGx::`colDataMap<-`,LongTableDataMapper-method]\n\t'
    rawdataCols <- colnames(rawdata(object))

    # -- Handle error conditions
    if (length(value) > 2 || !is.list(value)) {
        .error(funContext, 'Assignments to colDataMap should be a list ',
            'of length 2, where the first item is the name of the id columns ',
            'and the second item is the name of the metadata columns which ',
            'map to those id columns.')
    }

    hasIDcols <- value[[1]] %in% rawdataCols
    if (!all(hasIDcols) && length(rawdata(object))) {
        .error(funContext, 'One or more of the id columns specified ',
            'in value[[1]] are not valid column names in the rawdata slot of ',
            'this ', class(object)[1], ' object!')
    }

    if (length(value) > 1 && length(value[[2]]) != 0 &&
            length(rawdata(object))) {
        hasMetaCols <- value[[2]] %in% rawdataCols
        if (!all(hasMetaCols)) {
            .error(funContext,
                'The follow metadata columns in value[[2]] ',
                'are not present in rawdata(object): ',
                .collapse(value[[2]][!hasMetaCols]), '!')
        }
        hasOneToOneRelationship <-
            value[[2]] %in% cardinality(rawdata(object), group=value[[1]])
        if (!all(hasOneToOneRelationship)) {
            .error(funContext, 'The columns ',
                .collapse(value[[2]][!hasOneToOneRelationship]),
                ' do not have a 1:1 relationship with the specified ID ',
                'columns!')
        }
    }

    # -- Function body
    object@colDataMap <- value
    return(object)
})


##
## -- colData


#' Convenience method to subset the `colData` out of the `rawdata` slot using
#'   the assigned `colDataMap` metadata.
#'
#' @param x `LongTableDataMapper` object with valid data in the `rawdata` and
#'   `colDataMap` slots.
#' @param key `logical(1)` Should the table be keyed according to the
#'   `id_columns` of the `colDataMap` slot? This will sort the table in memory.
#'   Default is TRUE.
#'
#' @return `data.table` The `colData` as specified in the `colDataMap` slot.
#'
#' @export
setMethod("colData", signature("LongTableDataMapper"), function(x, key=TRUE) {
    .get_dimData(x, key, dim="col")
})


## ----------------
## ---- assayMap slot


#' @export
setGeneric('assayMap', function(object, ...) standardGeneric('assayMap'))

.docs_LongTableDataMapper_get_assayMap <- function(...) .parseToRoxygen(
    "
    @details
    __assayMap__:  A `list` of character vectors. The name of each list item
    will be the assay in a `LongTableDataMapper` object that the columns in the
    `character` vector will be assigned to. Column renaming occurs automatically
    when the character vectors have names (from the value to the name).

    @examples
    assayMap({data_})

    @md
    @aliases assayMap,{class_},list-method assayMap,{class_},List-method
    assayMap
    @exportMethod assayMap
    ",
    ...
)

#' @rdname LongTableDataMapper-accessors
#' @eval .docs_LongTableDataMapper_get_assayMap(class_=.local_class_3, data_=.local_data_3)
setMethod('assayMap', signature(object='LongTableDataMapper'),
        function(object) {
    object@assayMap
})


#' @export
setGeneric('assayMap<-', function(object, ..., value) standardGeneric('assayMap<-'))

#' @noRd
.docs_LongTableDataMapper_set_assayMap <- function(...) .parseToRoxygen(
    "
    @details
    __assayMap<-__: Updates the `@assaMap` slot of a `{class_}` object,
    returning an invisible NULL. Arguments:
    - value:  A `list` of character vectors, where the name of each list
    item is the name of an assay and the values of each character vector
    specify the columns mapping to the assay in the `S4` object the
    `{class_}` constructs.

    @examples
    assayMap({data_}) <- list(sensitivity=c(viability1='viability'))

    @md
    @aliases assayMap<-,{class_},list-method assayMap<-,{class_},List-methhod
    assayMap<-
    @exportMethod assayMap<-
    ",
    ...
)

#' @rdname LongTableDataMapper-accessors
#' @eval .docs_LongTableDataMapper_set_assayMap(class_=.local_class_3, data_=.local_data_3)
setReplaceMethod('assayMap', signature(object='LongTableDataMapper',
        value='list_OR_List'), function(object, value) {
    funContext <- '[CoreGx::`assayMap<-,LongTableDataMapper-method`]\n\t'
    rawdataCols <- colnames(rawdata(object))
    if (length(names(value)) == 0) stop(.errorMsg('The value argument must
        be a named list-like of column name character vectors!'))

    for (i in seq_along(value)) {
        hasRawdataCols <- unlist(value[[i]]) %in% rawdataCols
        if (!all(hasRawdataCols) && length(rawdata(object))) {
            stop(.errorMsg(funContext, 'There are no columns named ',
                .collapse(unlist(value[[i]])[!hasRawdataCols]),
                ' in the rawdata of this ', class(object)[1],
                ' object. Please ensure item ',
                names(value)[i], ' of value has valid column names.'))
        }
    }

    object@assayMap <- value
    return(object)
})


#' Extract the data for an assay from a `LongTableDataMapper`
#'
#' @param x `LongTableDataMapper` The object to retrive assay data form according
#'   to the `assayMap` slot.
#' @param i `character(1)` Name of an assay in the `assayMap` slot of `x`.
#' @param withDimNames `logical(1)` For compatibility with
#'   `SummarizedExperiment::assay` generic. Not used.
#'
#' @return `data.table` Data for the specified assay extracted from the
#'   `rawdata` slot of `x`.
#'
#' @importFrom checkmate assertSubset assertCharacter
#' @keywords internal
setMethod("assay", signature(x="LongTableDataMapper"),
        function(x, i, withDimnames=TRUE) {

    # Input validation
    .assayMap <- assayMap(x)
    assertCharacter(i, max.len=1)
    assertSubset(i, names(.assayMap))

    # Execution context
    funContext <- paste0("\n[", getPackageName(), "::assay,", class(x)[1],
        "-method]")

    return(.get_dimDataFromMap(x, key=TRUE, .assayMap[[i]],
        funContext=funContext))
})


#' Extract the data for all assays from a `LongTableDataMapper`
#'
#' @param x `LongTableDataMapper` The object to retrive assay data form according
#'   to the `assayMap` slot.
#' @param withDimNames `logical(1)` For compatibility with
#'   `SummarizedExperiment::assay` generic. Not used.
#'
#' @return `list` Data for all assays extracted from the
#'   `rawdata` slot of `x` as a `list` of `data.tables`, where the `keys` for
#'   each table are their `id_columns`.
#'
#' @importFrom checkmate assertSubset assertCharacter
#' @keywords internal
setMethod("assays", signature(x="LongTableDataMapper"),
        function(x, withDimnames=TRUE) {
    lapply(names(assayMap(x)), FUN=assay, x=x)
})

# -- metadataMap


#' @export
setGeneric('metadataMap', function(object, ...) standardGeneric('metadataMap'))

#' @noRd
.docs_LongTableDataMapper_get_metadataMap <- function(...) .parseToRoxygen(
    "
    @details
    __metadataMap__:  A `list` of `character` vectors. Each item is an element
    of the constructed objects `@metadata` slot.

    @examples
    metadataMap({data_})

    @md
    @aliases metadataMap,{class_}-method metadataMap
    @exportMethod metadataMap
    ",
    ...
)

#' @rdname LongTableDataMapper-accessors
#' @eval .docs_LongTableDataMapper_get_metadataMap(class_=.local_class_3,
#' data_=.local_data_3)
setMethod('metadataMap', signature(object='LongTableDataMapper'),
        function(object) {
    object@metadataMap
})


#' @export
setGeneric('metadataMap<-', function(object, ..., value)
    standardGeneric('metadataMap<-'))

#' @noRd
.docs_LongTableDataMapper_set_metadataMap <- function(...) .parseToRoxygen(
    "
    @details
    __metadataMap<-__: Updates `{class_}` object in-place, then returns an
    `invisible(NULL)`. Arguments:
    - value:  A `list` of `character` vectors. The name of each list item
    is the name of the item in the `@metadata` slot of the `{class_}` object
    created when `metaConstruct` is called on the `DataMapper`, and a
    character vector specifies the columns of `@rawdata` to assign to each item.

    @examples
    metadataMap({data_}) <- list(object_metadata=c('metadata'))

    @md
    @aliases metadataMap<-,{class_}-method metadataMap<-
    @exportMethod metadataMap<-
    ",
    ...
)

#' @rdname LongTableDataMapper-accessors
#' @eval .docs_LongTableDataMapper_set_metadataMap(class_=.local_class_3,
#' data_=.local_data_3, col_='metadata')
setReplaceMethod('metadataMap', signature(object='LongTableDataMapper',
    value='list_OR_List'), function(object, value)
{
    funContext <- '[CoreGx::`metadataMap<-,LongTableDataMapper-method`]\n\t'
    rawdataCols <- colnames(rawdata(object))
    if (length(names(value)) == 0) stop(.errorMsg('The value argument must
        be a named list-like of column name character vectors!'))

    for (i in seq_along(value)) {
        hasRawdataCols <- value[[i]] %in% rawdataCols
        if (!all(hasRawdataCols) && length(rawdata(object))) {
            stop(.errorMsg(funContext, 'There are no columns named ',
                .collapse(value[[i]][!hasRawdataCols]), ' in the rawdata ',
                'of this ', class(object)[1], ' object. Please ensure item ',
                names(value)[i], ' of value has valid column names.'))
        }
    }

    object@metadataMap <- value
    return(object)
})