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
                paste0(strwrap(
                    paste0(assayM[[aName]][[1]], collapse=', '), exdent=2),
                collapse="\n    ")
            )
            cat('\n    values:',
                paste0(strwrap(
                    paste0(assayM[[aName]][[2]], collapse=', '), exdent=2),
                collapse="\n    ")
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