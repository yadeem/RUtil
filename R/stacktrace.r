#' Usage:
#' e = try_capture_stack({expr}, environment())
#' if (on.exception(e, var1.to.print, var2.to.print))
#'     return(error.code) # fail fast
#' # normal workflow
#' @export
on.exception = function(e, message = NULL) {
    if (!is.error(e)) return(FALSE)

    error.log("on.exception", "================================================")
    error.log("on.exception", "Exception thrown:")
    error.log("on.exception", as.character(e))
    error.log("on.exception", "Message: ", message)
    error.log("on.exception", "Stacktrace:")
    error.log("on.exception", "{rev(e$calls)}")
    error.log("on.exception", "================================================")

    return(TRUE)
}

#' nodoc
#' @export
printt.stack.frame = function(frame = sys.frame(sys.nframe() - 1)) {
    vars = ls(frame)
    for (var in vars) {
        value = get(var, envir = frame)

        if (is.character(value)) {
            str = paste0("\"", value, "\"")
        } else if (is.function(value)) {
            str = paste0(deparse(value), collapse = "\n\t")
        } else {
            str = tryCatch({
                paste0(capture.output(value), collapse = "\n")
            }, error = function(cond) {
                paste0("<Error> ", cond$message)
            })
        }
        info.log("printt.stack.frame", "{var} = {str}")
    }
}

#' nodoc
#' @export
printt.stack.trace = function(nskip = 0, print.stack.variable = TRUE) {

    if (print.stack.variable) {
        info.log("printt.stack.trace", "========== Variables ==========")
        frame = sys.frame(sys.nframe() - nskip - 1)
        printt.stack.frame(frame)
    }

    info.log("printt.stack.trace", "========== Stacktrace =========")

    calls = sys.calls()
    calls = rev(calls)
    start.call.index = min((nskip + 2), length(calls))
    for (i in start.call.index:length(calls)) {
        info.log("printt.stack.trace", format(calls[[i]]))
    }
}
