get.RcppPrjTmpl.dependencies = function(external.only = FALSE) {
    external.dependencies = c(
        "Rcpp",
        "devtools"
    )
    internal.dependencies = c(
        "Demeter"
    )
    if (external.only) return(external.dependencies)

    return(c(external.dependencies, internal.dependencies))
}

build.RcppPrjTmpl = function(solution.dir, clean.build = FALSE) {
    message("Building RcppPrjTmpl")

    dependencies = get.RcppPrjTmpl.dependencies()
    message("Loading dependencies: ", paste0(dependencies, collapse = ", "))
    failed.pkg = get.dependencies(dependencies)
    if (length(failed.pkg) > 0)
        stop("Failed to load dependencies: ", paste0(failed.pkg, collapse = ", "))

    if (clean.build) {
        message("Cleaning build outputs")
        ret = clean.rcpp.project(solution.dir, "RcppPrjTmpl")
        if (!ret) warning("Clean failed")
    }
    build.rcpp.project(solution.dir, "RcppPrjTmpl")
}
