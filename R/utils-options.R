## TODO:: Do we want to allow customizing the names for molecular data types?
#' CoreGx Package Options
#'
#' @description
#' A set of customizable options for the default behavior of CoreGx. Used
#' to set the default identifiers for various entities worked with
#' by the CoreGx package, including samples, treatments, and
#' tissues and assays.
#'
#' @details
#' ## Configurable Identifiers
#'
#' ### Entity Identifiers
#' This set of identifiers used to match to standardized columns used to
#' uniquely identify different entities within the object. Strings containing
#' "<n>" will have a numeric value inserted which corresponds to the index of
#' that identifier if for example there are multiple treatments or samples
#' in a given experiment. When only one of each identifier is present in the
#' experiment, the numeric portion will be excluded. If your configured entity
#' identifier doesn't contain "<n>" then the numeric index will be appended
#' to the end of the string.
#'
#' * sampleid: A string which contains the standardized identifier for samples
#' in the experiment. Default to "sample<n>id".
#' * treatmentid: A string which contains the standardized
#' identifer for treatments in the experiment. Defaults to "treatment<n>id".
#' * treatmentdose: A string which contains the standardized
#' identifier for dose in the experiment. Defaults to "treatment<n>dose".
#' * tissueid: A string or regex pattern which contains the standardized
#' identifier for doses of each treatment in the experiment. Defaults to
#' "tissue<n>id".
#'
#' ### Assay Identifers
#' The assay identifier are used to establish naming convetions for the
#' different kinds of data stored within a CoreSet and TreatmentResponseExperiment.
#' They are a set of prefixed and suffixes which are combined together so that
#' CoreGx understands where in the object to look for specific kinds of
#' measurements needed to compute various dose-response and drug synergy
#' metrics. Prefixes and suffixes are combined with an "_" to create the
#' assay names. For example, monotherapy viablity measurement would be
#' stored in an assay called: "<monotherapy>_<viability>", where the values
#' inside angle brackets are replaced with the configured string for each
#' prefix and suffix. It is recommended to leave these default unless you
#' are an advanced user of R and CoreGx.
#'
#' * raw: A string representing the standardized suffix for the raw data
#' assay in the experiment.
#' * viability: A string which contains the standardized suffix for viability
#' measurement assays in the experiment. Viability assays contain the
#' normalized treatment response values for samples in an experiment.
#' * profiles: A string representing the standardized suffix for profile
#' assays in the experiment. Profiles are summary metrics computed over
#' a viability assay and can include things like dose-response parameters
#' or curve summary metrics such as IC50, AAC, etc.
#' * monotherapy: A string representing the prefix for monotherapy assays
#' in the experiment. Defaults to "mono".
#' * combotherapy: A string representing the prefix for combination therapy
#' assays in the experiment. Defaults to "combo".
#'
#' @rdname CoreGx-options
NULL

#' Retrieve all options for CoreGx
#'
#' @examples
#' .getCoreGxOptions()
#'
#' @return
#' `list()` All configured options for the CoreGx pacakge.
#'
#' @describeIn CoreGx-options
#' @export
getCoreGxOptions <- function() {
    opts <- options()
    return(opts[grepl("^CoreGx", names(opts))])
}

#' Retrieve the configured sample identifier
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @describeIn CoreGx-options
#' @export
sampleIdentifier <- function() return(options()["CoreGx.sampleid"])

#' Retrieve the configure treatment identifer pattern
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @describeIn CoreGx-options
#' @export
treatmentIdentifier <- function() return(options()["CoreGx.sampleid"])

#' Retrieve the configued tissue identifer pattern
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @describeIn CoreGx-options
#' @export
tissueIdentifer <- function() return(options()["CoreGx.sampleid"])

#' Retrieve the configured viability assay suffix
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @describeIn CoreGx-options
#' @export
viabilitySuffix <- function() return(options()["CoreGx.viability_assay_suffix"])

#' Retrieve the configured profile assay suffix
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @describeIn CoreGx-options
#' @export
profileSuffix <- function() return(options()["CoreGx.profile_assay_suffix"])

#' Retrieve the configured monotherapy prefix
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @describeIn CoreGx-options
#' @export
monotherapyPrefix <- function() return(options()["CoreGx.monotherapy_assay_prefix"])

#' Retrieve the configured combotherapy prefix
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @describeIn CoreGx-options
#' @export
combotherapyPrefix <- function() return(options()["CoreGx.combotherapy_assay_prefix"])