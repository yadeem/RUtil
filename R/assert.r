#' nodoc
#' @export
assert = function(...) {
    args = list(...)
    arg.names = as.list(substitute(list(...)))[-1]
    for (i in seq_along(args)) {
        if (length(args[[i]]) == 0)
            stop("Assertion failed, expr is null or length 0: ", deparse(arg.names[[i]]))
        else if (!args[[i]])
            stop("Assertion failed: ", deparse(arg.names[[i]]))
    }
}

#' nodoc
#' @export
assert.not.null = function(...) {
    args = list(...)
    arg.names = as.list(substitute(list(...)))[-1]
    for (i in seq_along(args)) {
        if (is.null(args[[i]]))
            stop(deparse(arg.names[[i]]), " cannot be null")
    }
}
