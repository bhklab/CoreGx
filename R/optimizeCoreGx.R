#' @importFrom data.table setDTthreads as.data.table
#' @importFrom bench press mark
#' @importFrom parallel detectCores


#' @title A helper method to find the best multithreading configuration for your
#'   computer
#'
#' @param sample_data `TreatmentResponseExperiment`
#' @param set `logical(1)` Should the function modify your R environment
#'   with the predicted optimal settings? This changes the global state of your
#'   R session!
#' @param report `logical(1)` Should a `data.frame` of results be returned
#'   by number of threads and operation be returned? Defaults to `!set`.
#'
#' @return
#' If `set=TRUE`, modifies `data.table` threads via `setDTthreads()`, otherwise
#' displays a message indicating the optimal number of threads.
#' If `report=TRUE`, also returns a `data.frame` of the benchmark results.
#'
#' @md
#' @export
optimizeCoreGx <- function(sample_data, set=FALSE, report=!set) {
    ncores <- max(1, parallel::detectCores() - 2)
    nthread_range <- if (ncores == 1) 1 else c(1, seq(2, ncores, 2))
    old_threads <- data.table::getDTthreads()
    message("Benchmarking assay(sample_data, withDimnames=TRUE)...")
    assay_report <- bench::press(nthread=nthread_range, {
        data.table::setDTthreads(nthread)
        gc()
        bench::mark({ assay(sample_data, 1, withDimnames=TRUE); NA })
    })
    message("Benchmarking assays(sample_data)...")
    assays_report <- bench::press(nthread=nthread_range, {
        data.table::setDTthreads(nthread)
        gc()
        bench::mark({ assays(sample_data); NA })
    })
    message("Benchmarking reindex(sample_data)...")
    reindex_report <- bench::press(nthread=nthread_range, {
        data.table::setDTthreads(nthread)
        gc()
        bench::mark({ reindex(sample_data); NA })
    })
    report_table <- as.data.table(
        rbind(assay_report, assays_report, reindex_report)
    )[,
        .(expression=as.character(expression), nthread,
            min_sec=as.numeric(min), median_sec=as.numeric(median),
            total_sec=as.numeric(total_time),
            mem_alloc_mb=as.numeric(mem_alloc) / 1e6, `itr/sec`,  `gc/sec`)
    ]
    best_results <- report_table[,
        .SD[which.min(median_sec), .(time=median_sec, nthread=nthread)],
        by=expression
    ]
    optimal_cores <- best_results[,
        as.integer(as.data.table(table(nthread))[which.max(N), nthread])
    ]
    message("Optimal cores for your machine are: ", optimal_cores)
    if (set) {
        message("Setting optimal cores")
        data.table::setDTthreads(optimal_cores)
    } else {
        data.table::setDTthreads(old_threads)
    }
    if (report) return(as.data.frame(report_table)) else return(invisible(NULL))
}
