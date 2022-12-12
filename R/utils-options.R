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
#' This set of identifiers is used to match to standardized columns used to
#' uniquely identify different entities within the object. Strings containing
#' "<n>" will have a numeric value inserted which corresponds to the index of
#' that identifier if for example there are multiple treatments or samples
#' for each observation in a given experiment. When only one of each
#' identifier is present in the experiment, the numeric portion will be
#' excluded. If your configured entity identifier doesn't contain "<n>" then
#' the numeric index will be appended to the end of the string.
#'
#' * sampleid: A string which contains the standardized identifier for samples
#' in the experiment. Defaults to "sample<n>id".
#' * treatmentid: A string which contains the standardized
#' identifer for treatments in the experiment. Defaults to "treatment<n>id".
#' * treatmentdose: A string which contains the standardized
#' identifier for treatment doses in the experiment. Defaults to
#' "treatment<n>dose".
#' * tissueid: A string which contains the standardized
#' identifier for tissue type of samples in the experiment. Defaults to
#' "tissue<n>id".
#'
#' ### Assay Identifers
#' The assay identifiers are used to establish naming conventions for the
#' different kinds of data stored within a `CoreSet`` and
#' `TreatmentResponseExperiment`. They are a set of prefixed and suffixes
#' which are combined together so that CoreGx understands where in the object
#' to look for specific kinds of measurements needed to compute various
#' dose-response and drug synergy metrics. Prefixes and suffixes are combined
#' with an "_" to create the assay names. For example, monotherapy viablity
#' measurement would be stored in an assay called: "<monotherapy>_<viability>",
#' where the values inside angle brackets are replaced with the configured
#' string for each prefix and suffix. It is recommended to leave these default
#' unless you are an advanced user of R and CoreGx.
#'
#' * raw: A string representing the standardized suffix for the raw data
#' assay in the experiment. Defaults to "raw". When no prefix is present
#' then the suffix will be used alone (e.g., for all your raw data the assay
#' would simply be named "raw").
#' * viability: A string which contains the standardized suffix for viability
#' measurement assays in the experiment. Viability assays contain the
#' normalized treatment response values for samples in an experiment.
#' * profiles: A string representing the standardized suffix for profile
#' assays in the experiment. Profiles are summary metrics computed over
#' a viability assay and can include things like dose-response curve parameters
#' or curve summary metrics such as IC50, AAC, etc.
#' * monotherapy: A string representing the prefix for monotherapy assays
#' in the experiment. Defaults to "mono".
#' * combotherapy: A string representing the prefix for combination therapy
#' assays in the experiment. Defaults to "combo".
#'
#' @name CoreGx-options
#' @rdname CoreGx-options
NULL

#' Retrieve all configured options for CoreGx.
#'
#' @examples
#' getCoreGxOptions()
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
#' @examples
#' sampleIdentifier()
#'
#' @describeIn CoreGx-options
#' @export
sampleIdentifier <- function() return(.getPackageOption("sampleid"))

#' Retrieve the configured treatment identifer pattern
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @examples
#' treatmentIdentifier()
#'
#' @describeIn CoreGx-options
#' @export
treatmentIdentifier <- function() return(.getPackageOption("treatmentid"))

#' Retrieve the configured tissue identifer pattern
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @examples
#' tissueIdentifier()
#'
#' @describeIn CoreGx-options
#' @export
tissueIdentifer <- function() return(.getPackageOption("tissueid"))

#' Retrieve the configured viability assay suffix
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @examples
#' viabilitySuffix()
#'
#' @describeIn CoreGx-options
#' @export
viabilitySuffix <- function() return(.getPackageOption("viability_assay_suffix"))

#' Retrieve the configured profile assay suffix
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @examples
#' profileSuffix()
#'
#' @describeIn CoreGx-options
#' @export
profileSuffix <- function() return(.getPackageOption("profile_assay_suffix"))

#' Retrieve the configured monotherapy prefix
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @examples
#' monotherapyPrefix()
#'
#' @describeIn CoreGx-options
#' @export
monotherapyPrefix <- function()
    return(.getPackageOption("monotherapy_assay_prefix"))

#' Retrieve the configured combotherapy prefix
#'
#' @return
#' `character(1)` The configured pattern for sample identifiers in CoreGx.
#'
#' @examples
#' combotherapyPrefix()
#'
#' @describeIn CoreGx-options
#' @export
combotherapyPrefix <- function()
    return(.getPackageOption("combotherapy_assay_prefix"))

#' Helper method to allow direct import of options in package depending on
#' CoreGx
#' @noRd
.getPackageOption <- function(name, dflt="CoreGx") {
    # Find the package name of the calling function
    pkg <- environmentName(environment(eval(sys.call(-1)[[1]])))
    opt_name <- paste0(pkg, ".", name)
    opt <- options()[[opt_name]]
    if (is.null(opt)) { opt <- options()[[paste0(dflt, ".", name)]] }
    return(opt)
}