
#' nodoc
#' @export
save.session = function(file = ".RSession",
                        list = NULL,
                        verbose = TRUE,
                        ...) {

    if (!is.null(dev.list()))
        warning.log("save.session", "Open graphics devices will not be saved or restored.")

    if (verbose) info.log("save.session", "Saving search path...")
    .save.session.search <<- search()

    if (verbose) info.log("save.session", "Saving list of loaded packages...")
    .save.session.packages <<- .packages()

    if (verbose) info.log("save.session", "Saving all data...")
    if (is.null(list)) {
        list = ls(envir = .GlobalEnv, all.names = TRUE)
    } else {
        list = c(list, ".save.session.search", ".save.session.packages")
    }
    save(list = list, file = file, ...)

    if (verbose) info.log("save.session", "Done.")
}

#' nodoc
#' @export
restore.session = function(file = ".RSession", check.search.path = TRUE, verbose = TRUE, ...) {
    if (verbose) info.log("restore.session", "Loading all data...")
    load(file, envir = .GlobalEnv, ...)

    if (verbose) {
        info.log("restore.session", "Loading packages...")
        load.lib = function(lib) { library(lib, character.only = TRUE) }
    } else {
        load.lib = function(lib) { suppressMessages(library(lib, character.only = TRUE)) }
    }
    sapply(rev(.save.session.packages), load.lib)

    if (check.search.path) {
        if (verbose) info.log("restore.session", "Restoring search path...")

        pad = function(x, n) c(rep(NA, n - length(x)), x)
        current.search = search()[-1]
        saved.search = .save.session.search[-1]
        identical = pad(current.search, length(saved.search)) == saved.search
        for (i in saved.search[!identical]) {
            if (charmatch("file:", i, nomatch = FALSE))
                attach(sub("file:", "", i))
            else if (charmatch("package:", i, nomatch = FALSE))
                fatal.log("restore.session", "Somehow we missed loading package {i}")
            else
                do.call("attach", list(as.name(i)))
        }
    }

    rm(list = c(".save.session.packages", ".save.session.search"), envir = .GlobalEnv)
    if (verbose) info.log("restore.session", "Done.")
}