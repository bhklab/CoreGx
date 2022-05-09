# Navigating this file:
# - Slot section names start with ----
# - Method section names start with ==
#
# As a result, you can use Ctrl + f to find the slot or method you are looking
# for quickly, assuming you know its name.
#
# For example Ctrl + f '== molecularProfiles' would take you the molecularProfiles
# method, while Ctrl + f '---- molecularProfiles' would take you to the slot
# section.

#' @include CoreSet-class.R allGenerics.R LongTable-class.R
NULL

.local_class <- 'CoreSet'
.local_data <- 'clevelandSmall_cSet'

#### CoreGx dynamic documentation
####
#### Warning: for dynamic docs to work, you must set
#### Roxygen: list(markdown = TRUE, r6=FALSE)
#### in the DESCRPTION file!


# =======================================
# Accessor Method Documentation Object
# ---------------------------------------

#' @noRd
.docs_CoreSet_accessors <- function(...) .parseToRoxygen(
    "
    @title Accessing and modifying information in a `{class_}`

    @description
    Documentation for the various setters and getters which allow manipulation
    of data in the slots of a `{class_}` object.

    @param object A `{class_}` object.
    @param value See details.
    @param mDataType `character(1)` The name of a molecular datatype to access
    from the `molecularProfiles` of a `{class_}` object.
    @param assay `character(1)` A valid assay name in the `SummarizedExperiment`
    of `@molecularProfiles` of a {class_} object for data type `mDataType`.
    @param dimension See details.
    @param ... See details.

    @return Accessors: See details.
    @return Setters: An updated `{class_}` object, returned invisibly.
    ",
    ...
)

#' @name CoreSet-accessors
#' @eval .docs_CoreSet_accessors(class_='CoreSet')
#' @eval .parseToRoxygen("@examples data({data_})", data_=.local_data)
NULL


# ======================================
# Accessor Methods
# --------------------------------------


## ====================
## ---- annotation slot
## --------------------


##
## == annotation


#' @noRd
.docs_CoreSet_get_annotation <- function(...) .parseToRoxygen(
    "
    @details

    ## @annotation

    __annotation__: A `list` of {class_} annotations with items: 'name',
    the name of the object; 'dateCreated', date the object was created; 'sessionInfo',
    the `sessionInfo()` when the object was created; 'call', the R constructor call;
    and 'version', the object version.

    @examples

    ## @annotation

    annotation({data_})

    @md
    @importMethodsFrom BiocGenerics annotation
    @aliases annotation
    @exportMethod annotation
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_annotation(class_=.local_class, data_=.local_data)
setMethod('annotation', signature("CoreSet"), function(object) {
    object@annotation
})


#' @noRd
.docs_CoreSet_set_annotation <- function(...) .parseToRoxygen(
    "
    @details
    __annotation<-__: Setter method for the annotation slot. Arguments:
    - value: a `list` of annotations to update the {class_} with.

    @examples
    annotation({data_}) <- annotation({data_})

    @md
    @importMethodsFrom BiocGenerics annotation<-
    @aliases annotation<-
    @exportMethod annotation<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_annotation(class_=.local_class, data_=.local_data)
setReplaceMethod("annotation", signature("CoreSet", "list"),
    function(object, value)
{
    object@annotation <- value
    object
})


##
## == dateCreated


#' @export
setGeneric("dateCreated", function(object, ...) standardGeneric("dateCreated"))

.docs_CoreSet_get_dateCreated <- function(...) .parseToRoxygen(
    "
    @details
    ## @dateCreated
    __dateCreated__: `character(1)` The date the `{class_}` object was
    created, as returned by the `date()` function.
    @examples
    dateCreated({data_})

    @md
    @aliases dateCreated,{class_}-method dateCreated
    @exportMethod dateCreated
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_dateCreated(class_=.local_class, data_=.local_data)
setMethod('dateCreated', signature("CoreSet"), function(object) {
    object@annotation$dateCreated
})


#' @export
setGeneric("dateCreated<-", function(object, ..., value)
    standardGeneric("dateCreated<-"))

.docs_CoreSet_set_dateCreated <- function(...) .parseToRoxygen(
    "
    @details
    __dateCreated<-__: Update the 'dateCreated' item in the `annotation` slot of
    a `{class_}` object. Arguments:
    - value: A `character(1)` vector, as returned by the `date()` function.
    @examples
    ## dateCreated
    dateCreated({data_}) <- date()

    @md
    @aliases dateCreated<-,{class_}-method dateCreated<-
    @exportMethod dateCreated<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_dateCreated(class_=.local_class, data_=.local_data)
setReplaceMethod('dateCreated', signature(object="CoreSet", value="character"),
    function(object, value)
{
    ## TODO:: Error handling - do we ever want to allow a datetime object?
    funContext <- .funContext('dateCreated')
    if (length(value) > 1) .error(funContext, 'dateCreated must by a character
        vector of length 1, as returned by the `date()` function.')
    object@annotation$dateCreated <- value
    return(object)
})


##
## == name


#' @export
setGeneric("name", function(object, ...) standardGeneric("name"))

.docs_CoreSet_get_name <- function(...) .parseToRoxygen(
    "
    @details
    __name__: `character(1)` The name of the `{class_}`, retreived from
    the `@annotation` slot.

    @examples
    name({data_})

    @md
    @aliases name,{class_}-method name
    @exportMethod name
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_name(class_=.local_class, data_=.local_data)
setMethod('name', signature("CoreSet"), function(object){
    return(object@annotation$name)
})

#' @export
setGeneric("name<-", function(object, ..., value) standardGeneric("name<-"))

#' @noRd
.docs_CoreSet_set_name <- function(...) .parseToRoxygen(
    "
    @details
    __name<-__: Update the `@annotation$name` value in a `{class_}`  object.
    - value: `character(1)` The name of the `{class_}` object.

    @examples
    name({data_}) <- 'new_name'

    @md
    @aliases name<-,{class_},character-method name<-
    @exportMethod name<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_name(class_=.local_class, data_=.local_data)
setReplaceMethod('name', signature("CoreSet"), function(object, value) {
    object@annotation$name <- value
    return(object)
})


## ==============
## ---- sample slot


##
## == sampleInfo


#' @export
setGeneric("sampleInfo", function(object, ...) standardGeneric("sampleInfo"))

#' @noRd
.docs_CoreSet_get_sampleInfo <- function(...) .parseToRoxygen(
    "
    ## @sample
    @details
    __{sample_}Info__: `data.frame` Metadata for all sample in a `{class_}` object.

    @md
    @aliases
    sampleInfo,{class_}-method sampleInfo
    {sample_}Info,{class_}-method
    {sample_}Info
    @exportMethod sampleInfo
    ",
    ...
)


.local_sample <- "cell"

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sampleInfo(class_=.local_class, sample_=.local_sample)
setMethod("sampleInfo", "CoreSet", function(object) {
    object@sample
})
#' @export
cellInfo <- function(...) sampleInfo(...)

#' @export
setGeneric("sampleInfo<-", function(object, value) standardGeneric("sampleInfo<-"))

#' @noRd
.docs_CoreSet_set_sampleInfo <- function(...) .parseToRoxygen(
    "
    @details
    __sampleInfo<-__: assign updated sample annotations to the `{class_}`
    object.
    Arguments:
    - value: a `data.frame` object.
    @examples
    sampleInfo({data_}) <- sampleInfo({data_})

    @md
    @aliases
    sampleInfo<-,{class_},data.frame-method
    sampleInfo<-
    {sample_}Info<-,{class_},data.frame-method
    {sample_}Info<-
    @exportMethod sampleInfo<-
    ",
    ...
)


#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sampleInfo(class_=.local_class, data_=.local_data,
#'  sample_=.local_sample)
setReplaceMethod("sampleInfo", signature(object="CoreSet", value="data.frame"),
        function(object, value) {
    funContext <- .funContext('::sampleInfo')
    if (is.null(rownames(value)))
    .error(funContext, "Please provide the sampleid as rownames for the sample
        annotations")
    object@sample <- value
    object
})
#' @export
`cellInfo<-` <- function(object, value) `sampleInfo<-`(object, value=value)


##
## == sampleNames

## TODO: Implement an actual @sample slot instead of using @sample  and aliases



#' @noRd
.docs_CoreSet_get_sampleNames <- function(...) .parseToRoxygen(
    "
    @details
    __sampleNames__: `character` Retrieve the rownames of the `data.frame` in
    the `sample` slot from a {class_} object.
    @examples
    sampleNames({data_})

    @md
    @aliases
    sampleName,{class_}-method
    sampleNames
    {sample_}Name,{class_}-method
    {sample_}Names
    @exportMethod sampleNames
    ",
    ...
)

#' @importMethodsFrom Biobase sampleNames
#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sampleNames(class_=.local_class, data_=.local_data,
#' sample_=.local_sample)
setMethod("sampleNames", signature("CoreSet"), function(object) {
    rownames(sampleInfo(object))
})
#' @export
cellNames <- function(object) sampleNames(object)


#' @noRd
.docs_CoreSet_set_sampleNames <- function(...) .parseToRoxygen(
    "
    @details
    __sampleNames<-__: assign new rownames to the sampleInfo `data.frame` for
    a {class_} object.
    Arguments:
    - value: `character` vector of rownames for the `sampleInfo(object)` `data.frame`.
    @examples
    sampleNames({data_}) <- sampleNames({data_})

    @md
    @aliases
    sampleNames<-,{class_},list-method
    sampleNames<-
    {sample_}Names<-,{class_},list-method
    {sample_}Names<-
    @exportMethod sampleNames<-
    ",
    ...
)


#' @importMethodsFrom Biobase sampleNames<-
#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sampleNames(class_=.local_class, data_=.local_data,
#' sample_=.local_sample)
setReplaceMethod("sampleNames", signature(object="CoreSet", value="character"),
        function(object, value) {
    ## TODO: does updateSampleId also update slots other than sample?
    object <- updateSampleId(object, value)
    return(object)
})
#' @export
`cellNames<-` <- function(object, value) `sampleNames<-`(object, value=value)


## -------------------
## ---- treatment slot

## TODO: Implement an actual @treatment slot to replace @drug and @radiation

#
# == treatmentInfo

#' @export
setGeneric('treatmentInfo', function(object, ...)
    standardGeneric('treatmentInfo'))

#' @noRd
.docs_CoreSet_get_treatmentInfo <- function(...) .parseToRoxygen(
    "
    @details
    __treatmentInfo__: `data.frame` Metadata for all treatments in a `{class_}`
    object. Arguments:
    - object: `{class_}` An object to retrieve treatment metadata from.

    @examples
    treatmentInfo({data_})

    @md
    @aliases treatmentInfo,{class_}-method treatmentInfo
    @exportMethod treatmentInfo
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_treatmentInfo(class_=.local_class, data_=.local_data)
setMethod('treatmentInfo', signature('CoreSet'), function(object) {
    treatmentType <- switch(class(object)[1],
        'PharmacoSet'='drug',
        'ToxicoSet'='drug',
        'RadioSet'='radiation',
        'CoreSet'='treatment'
    )
    package <- gsub('Set', 'Gx', class(object)[1])
    if ("treatment" %in% slotNames(object)) return(object@treatment)
    treatmentInfo <- get(paste0(treatmentType, 'Info'),
        envir=asNamespace(package))
    treatmentInfo(object)
})

#' @export
setGeneric('treatmentInfo<-', function(object, ..., value)
    standardGeneric('treatmentInfo<-'))

#' @noRd
.docs_CoreSet_set_treatmentInfo <- function(...) .parseToRoxygen(
    "
    @details
    __treatmentInfo<-__: `{class_}` object with updated treatment metadata.
    object. Arguments:
    - object: `{class_}` An object to set treatment metadata for.
    - value: `data.frame` A new table of treatment metadata for `object`.

    @examples
    treatmentInfo({data_}) <- treatmentInfo({data_})

    @md
    @aliases treatmentInfo<-,{class_},data.frame-method treatmentInfo<-
    @exportMethod treatmentInfo<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_treatmentInfo(class_=.local_class, data_=.local_data)
setReplaceMethod('treatmentInfo', signature(object='CoreSet',
        value='data.frame'), function(object, value) {
    object@treatment <- value
    return(invisible(object))
})

##
## == treatmentNames


#' @export
setGeneric('treatmentNames', function(object, ...)
    standardGeneric('treatmentNames'))

#' @noRd
.docs_CoreSet_get_treatmentNames <- function(...) .parseToRoxygen(
    "
    @details
    __treatmentNames__: `character` Names for all treatments in a `{class_}`
    object. Arguments:
    - object: `{class_}` An object to retrieve treatment names from.

    @examples
    treatmentNames({data_})

    @md
    @aliases treatmentNames,{class_}-method treatmentNames
    @exportMethod treatmentNames
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_treatmentNames(class_=.local_class, data_=.local_data)
setMethod('treatmentNames', signature(object='CoreSet'), function(object) {
    rownames(treatmentInfo(object))
})


#' @export
setGeneric('treatmentNames<-', function(object, ..., value)
    standardGeneric('treatmentNames<-'))

#' @noRd
.docs_CoreSet_set_treatmentNames <- function(...) .parseToRoxygen(
    "
    @details
    __treatmentNames<-__: `{class_}` Object with updates treatment names.
    object. Arguments:
    - object: `{class_}` An object to set treatment names from.
    - value: `character` A character vector of updated treatment names.

    @examples
    treatmentNames({data_}) <- treatmentNames({data_})

    @md
    @aliases treatmentNames<-,{class_},character-method treatmentNames<-
    @exportMethod treatmentNames<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_treatmentNames(class_=.local_class, data_=.local_data)
setReplaceMethod('treatmentNames',
        signature(object='CoreSet', value='character'),
        function(object, value) {
    object <- updateTreatmentId(object, new.ids=value)
    return(invisible(object))
})

## ------------------
## ---- curation slot


##
## == curation


#' @export
setGeneric("curation", function(object, ...) standardGeneric("curation"))

#' @noRd
.docs_CoreSet_get_curation <- function(...) .parseToRoxygen(
    "
    @details
    ## @curation
    __curation__: A `list` of curated mappings between identifiers in the
    {class_} object and the original data publication. {details_}
    @examples
    ## curation
    curation({data_})

    @md
    @aliases curation,{class_}-method curation
    @exportMethod curation
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_curation(class_=.local_class, data_=.local_data,
#' details_="Contains two `data.frame`s, 'sample' with sample ids and
#' 'tissue' with tissue ids.")
setMethod('curation', signature(object="CoreSet"), function(object) {
    object@curation
})

#' @export
setGeneric("curation<-", function(object, ..., value)
    sstandardGeneric("curation<-"))

#' @noRd
.docs_CoreSet_set_curation <- function(...) .parseToRoxygen(
    "
    @details
    __curation<-__: Update the `curation` slot of a {class_} object. Arugments:
    - value: A `list` of `data.frame`s, one for each type of curated
    identifier. {details_}
    @examples
    curation({data_}) <- curation({data_})

    @md
    @aliases curation<-,{class_},list-method curation<-
    @exportMethod curation<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_curation(class_=.local_class, data_=.local_data,
#' details_="For a `CoreSet` object the slot should contain tissue and
#' sample id `data.frame`s.")
setReplaceMethod("curation", signature(object="CoreSet", value="list"),
    function(object, value)
{
    object@curation <- value
    object
})



## ----------------------
## ---- datasetType slot


#
# == datasetType


#' @export
setGeneric("datasetType", function(object, ...) standardGeneric("datasetType"))

#' @noRd
.docs_CoreSet_get_datasetType <- function(...) .parseToRoxygen(
    "
    @details
    ## datasetType slot
    __datasetType__: `character(1)` The type treatment response in the
    `sensitivity` slot. Valid values are 'sensitivity', 'perturbation' or 'both'.
    @examples
    datasetType({data_})

    @md
    @aliases datasetType,{class_}-method datasetType
    @exportMethod datasetType
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_datasetType(class_=.local_class, data_=.local_data)
setMethod("datasetType", signature("CoreSet"), function(object) {
    object@datasetType
})


#' @export
setGeneric("datasetType<-",  function(object, value)
    standardGeneric("datasetType<-"))

#' @noRd
.docs_CoreSet_set_datasetType <- function(...) .parseToRoxygen(
    "
    @details
    __datasetType<-__: Update the datasetType slot of a {class_} object.
    Arguments:
    - value: A `character(1)` vector with one of 'sensitivity', 'perturbation'
    or 'both'
    @examples
    datasetType({data_}) <- 'both'

    @md
    @aliases datasetType<-,{class_},character-method datasetType<-
    @export
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_datasetType(class_=.local_class, data_=.local_data)
setReplaceMethod("datasetType", signature(object="CoreSet", value='character'),
    function(object, value)
{
    funContext <- .funContext('::datasetType,CoreSet,character-method')
    if (length(value) > 1) .error(funContext,
        'datasetType must be a character vector of length 1.')
    if (!is.element(value, c('sensitivity', 'perturbation', 'both')))
        .error(funContext, 'datasetType must be one of "sensitivity",
            "perturbation" or "both".')
    object@datasetType <- value
    object
})



## ---------------------------
## ---- molecularProfiles slot


##
## == molecularProfiles


#' @export
setGeneric("molecularProfiles", function(object, mDataType, assay, ...)
    standardGeneric("molecularProfiles"))

#' @noRd
.docs_CoreSet_get_molecularProfiles <- function(...) .parseToRoxygen(
    "
    @details
    ## @molecularProfiles
    __molecularProfiles__: `matrix()` Retrieve an assay in a
    `SummarizedExperiment` from the `molecularProfiles` slot of a `{class_}`
    object with the specified `mDataType`. Valid `mDataType` arguments can be
    found with `mDataNames(object)`. Arguments:
    - assay: Optional `character(1)` vector specifying an assay in the
    `SummarizedExperiment` of the `molecularProfiles` slot of the
    `{class_}` object for the specified `mDataType`. If excluded,
    defaults to modifying the first assay in the `SummarizedExperiment` for
    the given `mDataType`.

    @md
    @aliases molecularProfiles,{class_}-method molecularProfiles
    @importClassesFrom S4Vectors DataFrame List
    @importFrom S4Vectors DataFrame
    @importFrom SummarizedExperiment colData assay assayNames
    @exportMethod molecularProfiles
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_molecularProfiles(class_=.local_class, data_=.local_data)
setMethod(molecularProfiles, "CoreSet", function(object, mDataType, assay){
  funContext <- .funContext('::molecularProlfiles,CoreSet-method')
  ## TODO:: Add an all option that returns a list?
  if (mDataType %in% names(object@molecularProfiles)) {
    if (!missing(assay)) {
      if (assay %in% assayNames(object@molecularProfiles[[mDataType]])) {
        return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay))
      } else {
        .error(funContext, (paste('Assay', assay, 'not found in the SummarizedExperiment object!')))
      }
    } else {
      return(SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], 1))
    }
  } else {
    stop(paste0('mDataType ', mDataType, ' not found the object!'))
  }
})

#' @export
setGeneric("molecularProfiles<-", function(object, mDataType, assay, value)
    standardGeneric("molecularProfiles<-"))

#' @noRd
.docs_CoreSet_set_molecularProfiles <- function(...) .parseToRoxygen(
    "
    @details
    __molecularProfiles<-__: Update an assay in a `SummarizedExperiment` from
    the `molecularProfiles` slot of a {class_} object with the specified
    `mDataType`. Valid `mDataType` arguments can be found with
    `mDataNames(object)`.
    - assay: Optional `character(1)` vector specifying an assay in the
    `SummarizedExperiment` of the `molecularProfiles` slot of the
    `{class_}` object for the specified `mDataType`. If excluded,
    defaults to modifying the first assay in the `SummarizedExperiment` for
    the given `mDataType`.
    - value: A `matrix` of values to assign to the `assay` slot of the
    `SummarizedExperiment` for the selected `mDataType`. The rownames and
    column names must match the associated `SummarizedExperiment`.
    @examples
    # No assay specified
    molecularProfiles({data_}, 'rna') <- molecularProfiles({data_}, 'rna')

    # Specific assay
    molecularProfiles({data_}, 'rna', 'exprs') <-
        molecularProfiles({data_}, 'rna', 'exprs')

    @md
    @aliases molecularProfiles<-,{class_},character,character,matrix-method
    molecularProfiles<-,{class_},character,missing,matrix-method
    molecularProfiles<-
    @importFrom SummarizedExperiment assay
    @exportMethod molecularProfiles<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_molecularProfiles(class_=.local_class, data_=.local_data)
setReplaceMethod("molecularProfiles", signature(object="CoreSet",
    mDataType ="character", assay="character", value="matrix"),
    function(object, mDataType, assay, value)
{
  if (mDataType %in% names(object@molecularProfiles)) {
    SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], assay) <- value
  }
  object
})
setReplaceMethod("molecularProfiles",
    signature(object="CoreSet", mDataType ="character", assay="missing",
        value="matrix"), function(object, mDataType, assay, value)
{
  if (mDataType %in% names(object@molecularProfiles)) {
    SummarizedExperiment::assay(object@molecularProfiles[[mDataType]], 1) <- value
  }
  object
})


##
## == featureInfo


#' @export
setGeneric("featureInfo", function(object, mDataType, ...)
    standardGeneric("featureInfo"))

#' @noRd
.docs_CoreSet_get_featureInfo <- function(...) .parseToRoxygen(
    "
    @details
    __featureInfo__: Retrieve a `DataFrame` of feature metadata for the specified
    `mDataType` from the `molecularProfiles` slot of a `{class_}` object. More
    specifically, retrieve the `@rowData` slot from the `SummarizedExperiment`
    from the `@molecularProfiles` of a `{class_}` object with the name
    `mDataType`.
    @examples
    featureInfo({data_}, 'rna')

    @md
    @aliases featureInfo,{class_}-method featureInfo
    @importFrom SummarizedExperiment rowData rowData<-
    @exportMethod featureInfo
    ",
    ...
)


## FIXME: Why return NULL and not throw and error instead? Or at least a warning.
#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_featureInfo(class_=.local_class, data_=.local_data)
setMethod(featureInfo, "CoreSet", function(object, mDataType) {
  if (mDataType %in% names(object@molecularProfiles)) {
    return(rowData(object@molecularProfiles[[mDataType]]))
  } else{
    return(NULL)
  }
})

#' @export
setGeneric("featureInfo<-", function(object, mDataType, value)
    standardGeneric("featureInfo<-"))

#' @noRd
.docs_CoreSet_set_featureInfo <- function(...) .parseToRoxygen(
    "
    @details
    __featureInfo<-__: Update the `featureInfo(object, mDataType)` `DataFrame`
    with new feature metadata. Arguments:
    - value: A `data.frame` or `DataFrame` with updated feature metadata for
    the specified molecular profile in the `molecularProfiles` slot of a
    `{class_}` object.
    @examples
    featureInfo({data_}, '{mDataType_}') <- featureInfo({data_}, '{mDataType_}')

    @aliases featureInfo<-,{class_},character,data.frame-method
    featureInfo<-,{class_},character,DataFrame-method featureInfo<-
    @importFrom SummarizedExperiment rowData rowData<-
    @importFrom S4Vectors DataFrame
    @exportMethod featureInfo<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_featureInfo(class_=.local_class, data_=.local_data,
#'   mDataType_='rna')
setReplaceMethod("featureInfo", signature(object="CoreSet",
    mDataType ="character",value="data.frame"),
    function(object, mDataType, value)
{
    if(mDataType %in% names(object@molecularProfiles)){
        rowData(object@molecularProfiles[[mDataType]]) <-
            S4Vectors::DataFrame(value, rownames = rownames(value))
    }
    object
})
setReplaceMethod("featureInfo", signature(object="CoreSet",
    mDataType ="character",value="DataFrame"),
    function(object, mDataType, value)
{
  if(mDataType %in% names(object@molecularProfiles)){
    rowData(object@molecularProfiles[[mDataType]]) <-
        S4Vectors::DataFrame(value, rownames = rownames(value))
  }
  object
})


##
## == phenoInfo


#' @export
setGeneric("phenoInfo", function(object, mDataType, ...)
    standardGeneric("phenoInfo"))

#' @noRd
.docs_CoreSet_get_phenoInfo <- function(...) .parseToRoxygen(
    "
    @details
    __phenoInfo__: Return the `@colData` slot from the `SummarizedExperiment` of
    `mDataType`, containing sample-level metadata, from a `{class_}` object.

    @examples
    phenoInfo({data_}, '{mDataType_}')

    @md
    @importFrom SummarizedExperiment colData
    @aliases phenoInfo
    @exportMethod phenoInfo
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_phenoInfo(class_=.local_class, data_=.local_data, mDataType_='rna')
setMethod(phenoInfo, signature(object='CoreSet', mDataType='character'),
    function(object, mDataType)
{
    if (mDataType %in% mDataNames(object)) { # Columns = Samples
        return(colData(object@molecularProfiles[[mDataType]]))
    }else{
        ## FIXME:: Is there a reason we throw a NULL instead of an error?
        return(NULL)
    }
})

#' @export
setGeneric("phenoInfo<-", function(object, mDataType, value)
    standardGeneric("phenoInfo<-"))

#' @noRd
.docs_CoreSet_set_phenoInfo <- function(...) .parseToRoxygen(
    "
    @details
    __phenoInfo<-__: Update the `@colData` slot of the `SummarizedExperiment`
    of `mDataType` in the `@molecularProfiles` slot of a `{class_}` object.
    This updates the sample-level metadata in-place.
    - value: A `data.frame` or `DataFrame` object where rows are samples
    and columns are sample metadata.

    @examples
    phenoInfo({data_}, '{mDataType_}') <- phenoInfo({data_}, '{mDataType_}')

    @md
    @importFrom SummarizedExperiment colData colData<-
    @importFrom S4Vectors DataFrame
    @aliases phenoInfo<-,{class_},character,data.frame-method
    phenoInfo<-,{class_},character,DataFrame-method phenoInfo<-
    @exportMethod phenoInfo<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_phenoInfo(class_=.local_class, data_=.local_data, mDataType_='rna')
setReplaceMethod("phenoInfo", signature(object="CoreSet", mDataType ="character",
    value="data.frame"), function(object, mDataType, value)
{
    if(mDataType %in% mDataNames(object)) {
        colData(object@molecularProfiles[[mDataType]]) <-
            DataFrame(value, rownames = rownames(value))
    }
    object
})
setReplaceMethod("phenoInfo", signature(object="CoreSet",
    mDataType ="character", value="DataFrame"),
    function(object, mDataType, value)
{
    if (mDataType %in% mDataNames(object)) {
        colData(object@molecularProfiles[[mDataType]]) <- value
    }
    object
})


##
## == fNames

#' @export
setGeneric('fNames', function(object, mDataType, ...) standardGeneric('fNames'))

#' @noRd
.docs_CoreSet_get_fNames <- function(...) .parseToRoxygen(
    "
    @details
    __fNames__: `character()` The features names from the `rowData` slot of a
    `SummarizedExperiment` of `mDataType` within a `{class_}` object.

    @examples
    fNames({data_}, '{mDataType_}')

    @md
    @aliases fNames,{class_},character-method fNames
    @exportMethod fNames
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_fNames(class_=.local_class, data_=.local_data,
#' mDataType_='rna')
setMethod('fNames', signature(object='CoreSet', mDataType='character'),
    function(object, mDataType)
{
    rownames(featureInfo(object, mDataType))
})


#' @export
setGeneric('fNames<-', function(object, mDataType, ..., value)
    standardGeneric('fNames<-'))

#' @noRd
.docs_CoreSet_set_fNames <- function(...) .parseToRoxygen(
    "
    @details
    __fNames__: Updates the rownames of the feature metadata (i.e., `rowData`)
    for a `SummarizedExperiment` of `mDataType` within a `{class_}` object.
    - value: `character()` A character vector of new features names for the
    `rowData` of the `SummarizedExperiment` of `mDataType` in the
    `@molecularProfiles` slot of a `{class_}` object. Must be the same
    length as `nrow(featureInfo(object, mDataType))`,
    the number of rows in the feature metadata.

    @examples
    fNames({data_}, '{mDataType_}') <- fNames({data_}, '{mDataType_}')

    @md
    @aliases fNames<-,{class_},character,character-method fNames<-
    @exportMethod fNames<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_fNames(class_=.local_class, data_=.local_data,
#' mDataType_='rna')
setReplaceMethod('fNames', signature(object='CoreSet', mDataType='character',
    value='character'), function(object, mDataType, value)
{
    rownames(featureInfo(object, mDataType)) <- value
    object
})


##
## == mDataNames


#' @export
setGeneric("mDataNames", function(object, ...) standardGeneric("mDataNames"))

#' @noRd
.docs_CoreSet_get_mDataNames <- function(...) .parseToRoxygen(
    "
    @details
    __mDataNames__: `character` Retrieve the names of the molecular data types
    available in the `molecularProfiles` slot of a `{class_}` object. These
    are the options which can be used in the `mDataType` parameter of various
    `molecularProfiles` slot accessors methods.
    @examples
    mDataNames({data_})

    @md
    @aliases mDataNames,{class_}-method mDataNames
    @exportMethod mDataNames
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_mDataNames(class_=.local_class, data_=.local_data)
setMethod("mDataNames", "CoreSet", function(object) {
    return(names(object@molecularProfiles))
})


#' @export
setGeneric("mDataNames<-", function(object, ..., value) standardGeneric("mDataNames<-"))

#' @noRd
.docs_CoreSet_set_mDataNames <- function(...) .parseToRoxygen(
    "
    @details
    __mDataNames__: Update the molecular data type names of the
    `molecularProfiles` slot of a {class_} object. Arguments:
    - value: `character` vector of molecular datatype names, with length
    equal to `length(molecularProfilesSlot(object))`.
    @examples
    mDataNames({data_}) <- mDataNames({data_})

    @md
    @aliases mDataNames<-,{class_},ANY-method mDataNames<-
    @exportMethod mDataNames<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_mDataNames(class_=.local_class, data_=.local_data)
setReplaceMethod("mDataNames", "CoreSet", function(object, value) {
    names(object@molecularProfiles) <- value
    return(object)
})

##
## == molecularProfilesSlot


#' @export
setGeneric("molecularProfilesSlot", function(object, ...)
    standardGeneric("molecularProfilesSlot"))

#' @noRd
.docs_CoreSet_get_molecularProfilesSlot <- function(...) .parseToRoxygen(
    "
    @details
    __molecularProfilesSlot__: Return the contents of the `@molecularProfiles`
    slot of a `{class_}` object. This will either be a `list` or
    `MultiAssayExperiment` of `SummarizedExperiment`s.

    @examples
    molecularProfilesSlot({data_})

    @md
    @aliases moleculerProfilesSlot,{class_}-method molecularProfilesSlot
    @exportMethod molecularProfilesSlot
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_molecularProfilesSlot(class_=.local_class, data_=.local_data)
setMethod("molecularProfilesSlot", signature("CoreSet"), function(object) {
    object@molecularProfiles
})


#' @export
setGeneric("molecularProfilesSlot<-",
           function(object, value) standardGeneric("molecularProfilesSlot<-"))

#' @noRd
.docs_CoreSet_set_molecularProfilesSlot <- function(...) .parseToRoxygen(
    "
    @details
    __molecularProfilesSlot<-__: Update the contents of the `@molecularProfiles`
    slot of a `{class_}` object. Arguemnts:
    - value: A `list` or `MultiAssayExperiment` of `SummarizedExperiment`s. The
    `list` and `assays` should be named for the molecular datatype in each
    `SummarizedExperiment`.

    @examples
    molecularProfilesSlot({data_}) <- molecularProfilesSlot({data_})

    @md
    @aliases molecularProfilesSlot<-,{class_},list-method
    molecularProfilesSlot<-{class_},MultiAssayExperiment-method
    molecularProfilesSlot<-
    @exportMethod molecularProfilesSlot<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_molecularProfilesSlot(class_=.local_class, data_=.local_data)
setReplaceMethod("molecularProfilesSlot", signature("CoreSet", "list_OR_MAE"),
                 function(object, value)
{
    # funContext <- .S4MethodContext('molecularProfilesSlot<-', class(object),
    #     class(value))
    # if (!is(value, class(object@molecularProfiles)[1])) .error(funContext,
    #     'The class of value must be the same as the current @molecularProfiles!')
    object@molecularProfiles <- value
    object
})


## ---------------------
## ---- sensitivity slot


#
# == sensitivityInfo

#' @noRd
.docs_CoreSet_get_sensitivityInfo <- function(...) .parseToRoxygen(
    "
    @details

    ## @treatmentResponse

    ### Arguments:
    - `dimension`: Optional `character(1)` One of 'treatment', 'sample' or
    'assay' to retrieve `rowData`, `colData` or the 'assay_metadata' assay from
    the `{class_}` `@sensitvity` `LongTable` object, respectively. Ignored with
    warning if `@treatmentResponse` is not a `LongTable` object.
    -  `...`: Additional arguments to the `rowData` or `colData`.
    `LongTable` methods. Only used if the sensitivity slot contains a
    `LongTable` object instead of a `list` and the `dimension` argument is
    specified.

    ### Methods:

    __sensitivityInfo__: `DataFrame` or `data.frame` of sensitivity drug combo
    by sample metadata for the `{class_}` object. When the `dimension`
    parameter is used, it allows retrieval of the dimension specific metadata
    from the `LongTable` object in `@treatmentResponse` of a {class_} object.

    @examples
    sensitivityInfo({data_})

    @md
    @aliases sensitivityInfo,{class_},missing-method
    sensitivityInfo,{class_},character-method
    @exportMethod sensitivityInfo
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivityInfo(class_=.local_class,
#' data_=.local_data)
setMethod(sensitivityInfo, signature("CoreSet"),
        function(object, dimension, ...) {
    funContext <- .funContext('::sensitivityInfo')
    # case where sensitivity slot is a LongTable
    if (is(sensitivitySlot(object), 'LongTable')) {
        if (!missing(dimension)) {
            switch(dimension,
                sample={ return(colData(sensitivitySlot(object), ...)) },
                treatment={ return(rowData(sensitivitySlot(object), ...)) },
                assay={ return(assay(sensitivitySlot(object), 'assay_metadata')) },
                .error(funContext, 'Invalid value for the dimension argument.
                    Please select on of "sample", "drug" or "assay'))
        } else {
            return(.rebuildInfo(sensitivitySlot(object)))
        }
    # sensitivity is a list
    } else {
        if (!missing(dimension))
            .warning(funContext,' The dimension argument is only valid if the
                sensitivity slot contains a LongTable object. Ignoring the
                dimension and ... parameters.')
        return(sensitivitySlot(object)$info)
    }
})


#' Replicate the $info slot in the old sensitivity list from the new LongTable
#'   object
#'
#' @param longTable `LongTable`
#'
#' @keywords internal
#' @importFrom MatrixGenerics colAlls
#' @importFrom data.table setkeyv merge.data.table `:=` setDF
#' @noRd
.rebuildInfo <- function(longTable) {

    # Extract the information needed to reconstruct the sensitivityInfo
    #   data.frame
    aidx <- which(assayNames(longTable) %in% "assay_metadata")
    if (!length(aidx)) aidx <- 1
    assayIndexDT <- assay(longTable, aidx, key=TRUE)
    if (aidx == 1) assayIndexDT <- assayIndexDT[, .(rowKey, colKey)]
    setkeyv(assayIndexDT, c('rowKey', 'colKey'))
    rowDataDT <- rowData(longTable, key=TRUE)
    setkeyv(rowDataDT, 'rowKey')
    colDataDT <- colData(longTable, key=TRUE)
    setkeyv(colDataDT, 'colKey')

    rowIDcols <- rowIDs(longTable)[!grepl('dose$', rowIDs(longTable))]
    colIDcols <- colIDs(longTable)
    rownameCols <- c(rowIDcols, colIDcols)

    # join the tables into the original data
    infoDT <- merge.data.table(assayIndexDT, rowDataDT, all.x=TRUE)
    setkeyv(infoDT, 'colKey')
    infoDT <- merge.data.table(infoDT, colDataDT, all.x=TRUE)[,
        -c('rowKey', 'colKey')
    ]
    infoDT <- tryCatch({
        infoDT[, .SD, .SDcols=!patterns('drug.*dose$')]
    }, error=function(e) infoDT)

    # determine which columns map 1:1 with new identifiers and subset to those
    infoDT_first <- infoDT[, head(.SD, 1), by=rownameCols]
    infoDT_last <- infoDT[, tail(.SD, 1), by=rownameCols]
    keepCols <- colnames(infoDT_first)[
        colAlls(infoDT_first == infoDT_last, na.rm=TRUE)
        ]
    infoDT_sub <- unique(infoDT[, ..keepCols])

    # pad the dropped NA values, if they exists
    if ("sensitiivtyInfo_NA" %in% names(metadata(longTable))) {
            na_info <- copy(metadata(longTable)$sensitivityInfo_NA)
        setnames(na_info, "drugid", "drug1id")
        na_info <- cbind(
            na_info,
            unique(infoDT_sub[, .SD, .SDcols=!patterns("^drug1id$|^cellid$|^replicate_id$|^rn$")])
        )
        na_info[, replicate_id := seq_len(.N), by=.(drug1id, cellid)]
        infoDT_sub <- rbind(infoDT_sub, na_info)
    }
    if ("experiment_metadata" %in% names(metadata(longTable))) {
        infoDT_sub <- cbind(
            infoDT_sub,
            as.data.table(metadata(longTable)$experiment_metadata)
        )
    }

    # rebuild the rownames
    idCols <- grep("^drug[0-9]*id", colnames(infoDT_sub), value=TRUE)
    infoDT_sub[, drugid := Reduce(.paste_slashes, mget(..idCols))]
    infoDT_sub[, drug_uid := Reduce(.paste_colon, mget(..rowIDcols))]
    infoDT_sub[, sample_uid := Reduce(.paste_colon, mget(..colIDcols))]
    infoDT_sub[, exp_id := Reduce(.paste_, .(drug_uid, sample_uid))]

    # convert to data.frame
    setDF(infoDT_sub, rownames=infoDT_sub$exp_id)
    return(infoDT_sub)
}

#' @noRd
.docs_CoreSet_set_sensitivityInfo <- function(...) .parseToRoxygen(
    "
    @details

    __sensitivityInfo__<-: Update the `@treatmentResponse` slot metadata for a
    `{class_}` object. When used without the `dimension` argument is behaves
    similar to the old {class_} implementation, where the `@treatmentResponse` slot
    contained a list with a `$info` `data.frame` item. When the `dimension`
    arugment is used, more complicated assignments can occur where 'sample'
    modifies the `@sensitvity` `LongTable` colData, 'drug' the rowData and
    'assay' the 'assay_metadata' assay.
    Arguments:
    - value: A `data.frame` of treatment response experiment metadata,
    documenting experiment level metadata (mapping to drugs and samples). If
    the `@treatmentResponse` slot doesn't contain a `LongTable` and `dimension` is
    not specified, you can only modify existing columns as returned by
    `sensitivityInfo(object)`.
    @examples
    sensitivityInfo({data_}) <- sensitivityInfo({data_})

    @md
    @aliases sensitivityInfo<-,{class_},missing,data.frame-method
    sensitvityInfo<-,{class_},character,data.frame-method
    @import data.table
    @exportMethod sensitivityInfo<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sensitivityInfo(class_=.local_class, data_=.local_data)
setReplaceMethod("sensitivityInfo", signature(object="CoreSet", value="data.frame"),
                function(object, dimension, ..., value) {

    funContext <- .funContext('::sensitivityInfo<-')
    if (is(sensitivitySlot(object), 'LongTable')) {
        # coerce to data.table
        if (!is.data.table(value)) value <- data.table(value)
        if (missing(dimension)) {
            valueCols <- colnames(value)
            # get existing column names
            rowDataCols <- colnames(rowData(object@treatmentResponse))
            colDataCols <- colnames(colData(object@treatmentResponse))
            # drop any value columns that don't already exist
            hasValueCols <- valueCols %in% c(rowDataCols, colDataCols)
            if (!all(hasValueCols))
                .message(funContext, 'Dropping columns ',
                    .collapse(valueCols[!hasValueCols]), ' from value. Currently
                    this setter only allows modifying existing columns when
                    @treatmentResponse is a LongTable. For more fine grained updates
                    please use the dimension argument.')
            # update the object
            rowData(object@treatmentResponse, ...) <-
                unique(value[, .SD, .SDcols=valueCols %in% rowDataCols])
            colData(object@treatmentResponse, ...) <-
                unique(value[, .SD, .SDcols=valueCols %in% colDataCols])
        } else {
            switch(dimension,
                drug={ rowData(object@treatmentResponse, ...) <- value },
                sample={ colData(object@treatmentResponse, ...) <- value },
                assay={ assay(object@treatmentResponse, 'assay_metadata') <- value },
                .error(funContext, 'Invalid argument to dimension parameter.
                    Please choose one of "sample", "drug" or "assay"'))
        }
    } else {
        if (!missing(dimension))
            .warning(funContext, 'The dimension argument is only valid if the
                sensitivity slot contains a LongTable object. Ignoring dimension
                and ... parameters.')
        object@treatmentResponse$info <- value
    }
    return(object)
})


#
# == sensitvityMeasures


#' @noRd
.docs_CoreSet_get_sensitivityMeasures <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityMeaures__: Get the 'sensitivityMeasures' available in a `{class_}`
    object. Each measure reprents some summary of sample sensitivity to a given
    drug, such as ic50, ec50, AUC, AAC, etc. The results are returned as a
    `character` vector with all available metrics for the PSet object.
    @examples
    sensitivityMeasures({data_}) <- sensitivityMeasures({data_})

    @md
    @exportMethod sensitivityMeasures
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivityMeasures(class_=.local_class, data_=.local_data)
setMethod(sensitivityMeasures, "CoreSet", function(object) {
    return(colnames(sensitivityProfiles(object)))
})

#' @noRd
.docs_CoreSet_set_sensitityMeasures <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityMeaures__: Update the sensitivity meaure in a `{class_}`
    object. Thesee values are the column names of the 'profiles' assay and
    represent various compued sensitviity metrics such as ic50, ec50, AUC, AAC,
    etc.
    - value: A `character` vector of new sensitivity measure names, the
    then length of the character vector must matcht he number of columns of the
    'profiles' assay, excluding metadata and key columns.
    @examples
    sensitivityMeasures({data_}) <- sensitivityMeasures({data_})

    @md
    @exportMethod sensitivityMeasures
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sensitityMeasures(class_=.local_class, data_=.local_data)
setReplaceMethod('sensitivityMeasures',
    signature(object='CoreSet', value='character'),
    function(object, value)
{
    colnames(sensitivityProfiles(object)) <- value
    object
})


#
# == sensitivityProfiles


#' @noRd
.docs_CoreSet_get_sensitivityProfiles <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityProfiles__: Return the sensitivity profile summaries from the
    sensitivity slot. This data.frame cotanins vaarious sensitivity summary
    metrics, such as ic50, amax, EC50, aac, HS, etc as columns, with rows as
    drug by sample experiments.
    @examples
    sensitivityProfiles({data_})

    @md
    @exportMethod sensitivityProfiles
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivityProfiles(class_=.local_class, data_=.local_data)
setMethod(sensitivityProfiles, "CoreSet", function(object) {
    funContext <- .funContext('::sensitivityProfiles')
    if (is(sensitivitySlot(object), 'LongTable')) {
        if (!('profiles' %in% assayNames(sensitivitySlot(object)))) {
            .error(funContext, 'The LongTable onject in the sensivitiy slot
                is not formatted correctly: it must contain an assay
                named "profiles"!')
        } else {
            .rebuildProfiles(sensitivitySlot(object))
        }
    } else {
        return(object@treatmentResponse$profiles)
    }
})

#' @keywords internal
.rebuildProfiles <- function(object) {
    profDT <- object$profiles
    rowCols <- rowIDs(object)
    colCols <- colIDs(object)
    profDT[, sample_uid := Reduce(.paste_colon, mget(colCols))]
    profDT[, drug_uid := Reduce(.paste_colon, mget(rowCols))]
    profDT[, exp_id := .paste_(drug_uid, sample_uid)]
    assayCols <- setdiff(colnames(assay(object, "profiles", raw=TRUE)), "profiles")
    sensProf <- as.data.frame(unique(profDT[, .SD, .SDcols=assayCols]))
    rownames(sensProf) <- unique(profDT$exp_id)
    return(sensProf)
}

#' @noRd
.docs_CoreSet_set_sensitivityProfiles <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityProfiles<-__: Update the sensitivity profile summaries the
    sensitivity slot. Arguments:
    -value: A `data.frame` the the same number of rows as as returned by
    `sensitivityProfiles(object)`, but potentially modified columns, such as the
    computation of additional summary metrics.
    @examples
    sensitivityProfiles({data_}) <- sensitivityProfiles({data_})

    @md
    @exportMethod sensitivityProfiles<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sensitivityProfiles(class_=.local_class, data_=.local_data)
setReplaceMethod("sensitivityProfiles",
    signature(object="CoreSet", value="data.frame"),
    function(object, value)
{
    if (is(sensitivitySlot(object), 'LongTable'))
        warning(.warnMsg("The ", class(object)[1], " class structure has been",
            " updated! Assignment via sensitivityProfiles no long works, please",
            " see vignette('The LongTable Class', package='CoreGx') for more",
            " information."))
    else
        object@treatmentResponse$profiles <- value
    return(object)
})


#
# == sensitivityRaw


#' @noRd
.docs_CoreSet_get_sensitivityRaw <- function(...) .parseToRoxygen(
    "
    @details
    __sensitivityRaw__: Access the raw sensitiity measurents for a {class_}
    object. A 3D `array` where rows are experiment_ids, columns are doses
    and the third dimension is metric, either 'Dose' for the doses used or
    'Viability' for the sample viability at that dose.
    @examples
    head(sensitivityRaw({data_}))

    @md
    @exportMethod sensitivityRaw
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivityRaw(class_=.local_class, data_=.local_data)
setMethod("sensitivityRaw", signature("CoreSet"), function(object) {
    if (is(sensitivitySlot(object), 'LongTable'))
        return(.rebuildRaw(sensitivitySlot(object)))
    else
        return(object@treatmentResponse$raw)
})

#' Replicate the $raw slot in the old @treatmentResponse list from a LongTable
#'
#' @param longTable `LongTable`
#'
#' @return A 3D `array` where rows are experiment_ids, columns are doses
#' and the third dimension is metric, either 'Dose' for the doses used or
#' 'Viability' for the sample viability at that dose.
#'
#' @keywords internal
#' @importFrom data.table merge.data.table dcast
#' @noRd
.rebuildRaw <- function(longTable) {

    ## TODO:: This function currently assumes there will only be one valid
    ## dose per drug combination, which may not be true.

    funContext <- .funContext(':::.rebuildRaw')
    if (!('sensitivity' %in% assayNames(longTable)))
        .error(funContext, 'There is no assay named sensitivity. Not sure
            how to retrieve sensitivityRaw without a sensitivity assay. Try
            renaming your assays in the @treatmentResponse LongTable object?')

    # Extract the information needed to reconstruct the sensitivityRaw array
    viability <- longTable$sensitivity

    # Early return for single drug sensitivity experimentss
    ## TODO:: refactor this into a helper?
    if ('assay_metadata' %in% assayNames(longTable) &&
        'old_column' %in% colnames(longTable$assay_metadata))
    {
        metadataDT <- copy(longTable$assay_metadata)
        sensitivityDT <- copy(longTable$sensitivity)
        # .NATURAL joins on all identical columns
        assayDT <- metadataDT[sensitivityDT, on=.NATURAL]
        if (length(colIDs(longTable)) > 1) {
            assayDT[, sampleid := Reduce(.paste_colon, mget(colIDs(longTable)))]
        }
        assayDT[, exp_id := paste0(drug1id, '_', sampleid)]
        .mean <- function(x) mean(as.numeric(x), na.rm=TRUE)
        doseDT <- dcast(assayDT, exp_id ~ old_column, value.var='drug1dose',
            fun.aggregate=.mean)
        viabDT <- dcast(assayDT, exp_id ~ old_column, value.var='viability',
            fun.aggregate=.mean)
        sensRaw <- array(dim=list(nrow(doseDT), ncol(doseDT) -1, 2),
            dimnames=list(doseDT$exp_id, colnames(doseDT)[-1],
                c('Dose', 'Viability')))
        sensRaw[, , 'Dose'] <- as.matrix(doseDT[, !'exp_id'])
        sensRaw[, , 'Viability'] <- as.matrix(viabDT[, !'exp_id'])
        return(sensRaw)
    }

    # Build the rownames
    .paste_colons <- function(...) paste(..., sep=':')
    viability[, row_ids := Reduce(.paste_colons, mget(rowIDs(longTable)))]
    viability[, col_ids := Reduce(.paste_colons, mget(colIDs(longTable)))]
    viability[, rownames := paste0(row_ids, '_', col_ids)]
    viability[, c('row_ids', 'col_ids') := NULL]

    # Merge the doses into vectors in a list column
    viability[, dose := Reduce(.paste_slashes, mget(colnames(.SD))),
        .SDcols=patterns('^.*dose$')]

    # Repeat the dose values if there are more viabilities
    numReplicates <- viability[, ncol(.SD), .SDcols=patterns('^viability.*')]
    if (numReplicates > 1) {
        viability[, paste0('dose', seq_len(numReplicates)) := dose]
        viability[, dose := NULL]
    }

    # Build the array
    sensRaw <- array(dim=list(nrow(viability), numReplicates, 2),
        dimnames=list(viability$rownames, paste0('dose', seq_len(numReplicates)),
            c('Dose', 'Viability')))
    sensRaw[, , 'Dose'] <- as.matrix(viability[, .SD,
        .SDcols=patterns('^dose.*')])
    sensRaw[, , 'Viability'] <- as.matrix(viability[, .SD,
        .SDcols=patterns('^viability.*')])

    return(sensRaw)
}

#' @noRd
.docs_CoreSet_set_sensitivityRaw <- function(...) .parseToRoxygen(
    "
    @details

    __sensitvityRaw<-__: Update the raw dose and viability data in a `{class_}`.
    - value: A 3D `array` object where rows are experiment_ids, columns are
    replicates and pages are c('Dose', 'Viability'), with the corresponding
    dose or viability measurement for that experiment_id and replicate.

    @examples
    sensitivityRaw({data_}) <- sensitivityRaw({data_})

    @md
    @importFrom data.table data.table as.data.table := merge.data.table tstrsplit
    @exportMethod sensitivityRaw<-
    "
    ,
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sensitivityRaw(class_=.local_class, data_=.local_data)
setReplaceMethod('sensitivityRaw', signature("CoreSet", "array"),
    function(object, value)
{
    funContext <- .funContext("::sensitivityRaw<-")
    if (is(sensitivitySlot(object), 'LongTable')) {

        ## TODO:: validate value
        longTable <- sensitivitySlot(object)

        viabilityCols <- colnames(assay(longTable, 'sensitivity',
            metadata=FALSE, key=FALSE))
        # Handle the non-drug combo case
        if (length(viabilityCols) != ncol(value)) {
            valueDT <- as.data.table(value)
            valueDT <- dcast(valueDT, V1 + V2 ~ V3, value.var='value')
            valueDT[, V2 := NULL] # drop the column names
            setnames(valueDT, old=c('Dose', 'Viability'),
                new=c('drug1dose', 'viability'))
            valueDT[, c('drug1id', 'sample_uid') := tstrsplit(V1, '_', type.convert=TRUE)]
            valueDT[, (colIDs(longTable)) := tstrsplit(sample_uid, ':', type.convert=TRUE)]
            valueDT[, c('V1', 'sample_uid') := NULL]
            assay(longTable, i='sensitivity') <- valueDT
        } else {
            # Process into a the proper format for the sensitivity assay
            # NOTE: as.matrix deals with the case where there is only a single
            #   viability column in the sensitivityRaw array object,
            #   in which case the drop=TRUE argument causes a vector to be
            #   returned
            raw <- as.data.table(as.matrix(value[, , 'Viability']),
                keep.rownames='rn', na.rm=FALSE)
            coerceCols <- colnames(raw)[-1]
            raw[, (coerceCols) := lapply(.SD, as.numeric), .SDcols=!'rn']
            raw[, c('row_id', 'col_id') := tstrsplit(rn, '_')]
            raw[, (rowIDs(longTable)) := tstrsplit(row_id, ':', type.convert=TRUE)]
            raw[, (colIDs(longTable)) := tstrsplit(col_id, ':', type.convert=TRUE)]
            raw[, c('row_id', 'col_id', 'rn') := NULL]
            colnames(raw) <- gsub('^dose|^V', 'viability', colnames(raw))

            # Update the assay
            assay(longTable, i='sensitivity') <- raw
        }

        # Update the object
        sensitivitySlot(object) <- longTable
    } else {
        if (!is.array(value)) .error(funContext, "Values assigned to the
            sensitivityRaw slot must be an array of experiment by dose by
            value!")
        object@treatmentResponse$raw <- value
        object
    }
    return(object)
})


#
# == sensitivitySlot


#' @export
setGeneric("sensitivitySlot", function(object, ...) standardGeneric("sensitivitySlot"))

#' @noRd
.docs_CoreSet_get_sensitivitySlot <- function(...) .parseToRoxygen(
    "
    __sensitivitySlot__: Retrive the contents of `@treatmentResponse` from a `{class_}`
    object.

    @examples
    sensitivitySlot({data_})

    @md
    @aliases sensitivitySlot,{class_}-method sensitivitySlot
    @exportMethod sensitivitySlot
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensitivitySlot(class_=.local_class,
#'   data_=.local_data)
setMethod("sensitivitySlot", signature("CoreSet"), function(object) {
    object@treatmentResponse
})


#' @export
setGeneric("sensitivitySlot<-", function(object, ..., value)
    standardGeneric("sensitivitySlot<-"))

.docs_CoreSet_set_sensitivitySlot <- function(...) .parseToRoxygen(
    "
    __sensitivitySlot<-__: Assign data to the `@treatmentResponse` slot of a
    `{class_}` object.
    - value: Either a `LongTable` class object, or a list with an 'info'
    `data.frame` of experiment metadata, 'profiles' `data.frame` with
    summary statistics from the sensitivity experiment and a 'raw' 3D array
    where rows are experiments, columns are replicates and pages are 'Dose'
    or 'Viability' containing their respective values for that drug by sample
    experiment. The type of `value` must match type of the current `@treatmentResponse`
    slot of the `{class_}` object.

    @examples
    sensitivitySlot({data_}) <- sensitivitySlot({data_})

    @md
    @aliases sensitivitySlot<- sensitivitySlot<-,{class_},list-method
    sensitivitySlot<-,{class_},LongTable-method
    @exportMethod sensitivitySlot<-
    ",
    ...
)


#' @rdname CoreSet-accessors
#' @include LongTable-class.R
#' @eval .docs_CoreSet_set_sensitivitySlot(class_=.local_class, data_=.local_data)
setReplaceMethod("sensitivitySlot", signature(object="CoreSet", value="list_OR_LongTable"),
    function(object, value)
{
    # funContext <- .S4MethodContext('sensitivitySlot<-', class(object), class(value))
    # ## TODO:: Maybe try coercing the list to a LongTable and vice versa?
    if (!is(object@treatmentResponse, class(value)[1])) .error(funContext, 'The types
        of the current and @treatmentResponse slot and the value parameter must be
        the same!')
    object@treatmentResponse <- value
    return(object)
})


##
## == sensNumber


#' @export
setGeneric("sensNumber", function(object, ...) standardGeneric("sensNumber"))

#' @noRd
.docs_CoreSet_get_sensNumber <- function(...) .parseToRoxygen(
    "
    @details
    __sensNumber__: Return a count of viability observations in a `{class_}`
    object for each drug-combo by sample combination.

    @examples
    sensNumber({data_})

    @md
    @aliases sensNumber
    @exportMethod sensNumber
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_sensNumber(class_=.local_class, data_=.local_data)
setMethod(sensNumber, "CoreSet", function(object){
    return(
        if (is(object@treatmentResponse, 'LongTable'))
            .rebuildSensNumber(object@treatmentResponse)
        else
            object@treatmentResponse$n
    )
})

.rebuildSensNumber <- function(object) {
    sensitivityDT <- object$sensitivity
    # Melt replicates so they get counted
    sensitivityDT_melted <- melt(sensitivityDT,
        measure=patterns('^viability'), variable.name='replicate',
        value.name='viability')

    # Determine the drug and sample combos, ignoring other identifiers
    .paste_colon <- function(x, y) paste(x, y, sep=':')
    drugidCols <- sensitivityDT[, colnames(.SD), .SDcols=patterns('drug.*id')]
    sampleidCols <- sensitivityDT[, colnames(.SD), .SDcols=patterns('sample.*id')]

    # Parse the columns to dcast by to get the counts
    sensitivityDT_melted[, .drugCombo := Reduce(.paste_colon, mget(drugidCols))]
    sensitivityDT_melted[, .sampleCombo := Reduce(.paste_colon, mget(sampleidCols))]

    # Count existing sensitivity measurements
    .count_not_NA <- function(x) sum(!is.na(x))
    sensNumbDT <- dcast(sensitivityDT_melted, .drugCombo ~ .sampleCombo,
        value.var='viability', fun.aggregate=.count_not_NA)
    sensNumberM <- as.matrix(sensNumbDT[, !'.drugCombo'])
    rownames(sensNumberM) <- sensNumbDT[['.drugCombo']]

    return(sensNumberM)

    ## TODO:: Pad for any missing drugs or samples
    allDrugCombos <- rowData(object)[, Reduce(.paste_colon, mget(drugidCols))]
    allSampleCombos <- colData(object)[, Reduce(.paste_colon, mget(sampleidCols))]

}

#' @export
setGeneric("sensNumber<-", function(object, value) standardGeneric("sensNumber<-"))

#' @noRd
.docs_CoreSet_set_sensNumber <- function(...) .parseToRoxygen(
    "
    @details
    __sensNumber<-__: Update the 'n' item, which holds a matrix with a count
    of drug by sample-line experiment counts, in the `list` in `@treatmentResponse`
    slot of a `{class_}` object. Will error when `@sensitviity` contains
    a `LongTable` object, since the counts are computed on the fly. Arguments:
    - value: A `matrix` where rows are samples and columns are drugs, with a
    count of the number of experiments for each combination as the values.

    @examples
    sensNumber({data_}) <- sensNumber({data_})

    @md
    @aliases sensNumber<-
    @exportMethod sensNumber<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_sensNumber(class_=.local_class, data_=.local_data)
setReplaceMethod('sensNumber', signature(object="CoreSet", value="matrix"),
    function(object, value)
{
    if (is(sensitivitySlot(object), 'LongTable')) {
        object
    } else {
        object@treatmentResponse$n <- value
        object
    }
})


## ======================
## ---- perturbation slot

##
## == pertNumber


#' @export
setGeneric("pertNumber", function(object, ...) standardGeneric("pertNumber"))

#' @noRd
.docs_CoreSet_get_pertNumber <- function(...) .parseToRoxygen(
    "
    @details
    __pertNumber__: `array` Summary of available perturbation experiments
    from in a `{class_}` object. Returns a 3D `array` with the number of
    perturbation experiments per drug and sample, and data type.

    @examples
    pertNumber({data_})

    @md
    @aliases pertNumber
    @exportMethod pertNumber
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_get_pertNumber(class_=.local_class, data_=.local_data)
setMethod(pertNumber, "CoreSet", function(object){
    return(object@perturbation$n)
})

#' @export
setGeneric("pertNumber<-", function(object, value) standardGeneric("pertNumber<-"))

.docs_CoreSet_set_pertNumber <- function(...) .parseToRoxygen(
    "
    @details
    __pertNumber<-__: Update the `@perturbation$n` value in a `{class_}` object,
    which stores a summary of the available perturbation experiments. Arguments:
    - value: A new 3D `array` with the number of perturbation experiments per
    drug and sample, and data type

    @examples
    pertNumber({data_}) <- pertNumber({data_})

    @md
    @aliases pertNumber<-
    @exportMethod pertNumber<-
    ",
    ...
)

#' @rdname CoreSet-accessors
#' @eval .docs_CoreSet_set_pertNumber(class_=.local_class, data_=.local_data)
setReplaceMethod('pertNumber', signature(object="CoreSet", value="array"),
    function(object, value)
{
  object@perturbation$n <- value
  object
})