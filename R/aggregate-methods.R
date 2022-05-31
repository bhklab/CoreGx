aggregate2 <- function(x, by, ..., nthread=1, BPPARAM=BiocParallel::bpparam()) {
    stopifnot(is.data.table(x))
    stopifnot(is.character(by) && all(by %in% colnames(x)))

    # -- capture dots as a call and parse dot names, adding them if they are missing
    agg_call <- substitute(list(...))
    dot_names <- names(agg_call)[-1L]
    if (is.null(dot_names)) dot_names <- rep(TRUE, length(agg_call) - 1)
    for (i in which(dot_names == "")) {
        dot_call <- agg_call[[i + 1]]
        # assumes the first argument in a function call is always the column name!
        dot_names[i] <- paste0(dot_call[1:max(2, length(dot_call))], collapse="_")
    }
    names(agg_call)[2L:length(agg_call)] <- dot_names

    if (nthread == 1) {
        res <- x[, eval(agg_call), by=c(by)]
    } else {
        x_split <- data.table:::split.data.table(x, by=by)
        BiocParallel::bpworkers(BPPARAM) <- nthread
        BiocParallel::bpprogressbar(BPPARAM) <- TRUE
        res <- BiocParallel::bplapply(
            x_split,
            function(x, agg_call, by) x[, eval(agg_call), by=c(by)],
            agg_call=agg_call, by=by,
            BPPARAM=BPPARAM
        )
        res <- rbindlist(res)
    }
    return(res)
}


if (Sys.frame() == 0) {
    debug(aggregate2)
    sens |>
        aggregate2(
            mv=mean(viability), mean(drug1dose),
            by=c("drug1id", "drug2id", "cellid"),
            nthread=10
        )

    sens[is.na(drug2dose)] |>
        aggregate2(
            auc=tryCatch(computeAUC(drug1dose, viability), error=\(e) NA),
            by=c("drug1id", "cellid")
        ) ->
        auc_dt
}