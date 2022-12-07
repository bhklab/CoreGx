.onLoad <- function(libname, pkgname) {
    # CoreGx nomenclature options
    cgx_opts <- list(
        CoreGx.treatmentid="treatment\\d+id",
        CoreGx.sampleid="sampleid",
        CoreGx.tissueid="tissueid",
        CoreGx.rawdata_assay="raw",
        CoreGx.monotherapy_assay_prefix="mono",
        CoreGx.combotherapy_assay_prefix="combo",
        CoreGx.viability_assay_suffix="viability",
        CoreGx.profile_assay_suffix="profiles"
    )
    opts <- options()
    # Allow users to override defaults in their .Rprofile
    unset_cgx_options=which(is.null(opts[names(cgx_opts)]))
    do.call(options, args=cgx_opts[unset_cgx_options])
}