get.rutil.dependencies = function(external.only = FALSE) {
    dependencies = c(
        "Rcpp",
        "devtools",
        "evaluate",
        "glue",
        "logger",
        "readr",
        "stringr"
    )
    return(dependencies)
}

build.rutil = function(solution.dir, clean.build = FALSE) {
    message("Building RUtil")

    dependencies = get.rutil.dependencies()
    message("Loading dependencies: ", paste0(dependencies, collapse = ", "))
    failed.pkg = get.dependencies(dependencies)
    if (length(failed.pkg) > 0)
        stop("Failed to load dependencies: ", paste0(failed.pkg, collapse = ", "))

    if (clean.build) {
        message("Cleaning build outputs")
        ret = clean.rcpp.project(solution.dir, "RUtil")
        if (!ret) warning("Clean failed")
    }
    build.rcpp.project(solution.dir, "RUtil")
}
