#' nodoc
#' @export
get.cmd.args = function() {
    if (exists("cmd.args", envir = .GlobalEnv)) {
        info.log("get.cmd.args", "Using global var cmd.args: ", paste0(cmd.args, collapse = " "))
        args = cmd.args
    } else {
        args = commandArgs(trailingOnly = T)
    }

    args.list = list()
    for (arg in args) {
        if (grepl("=", arg)) {
            key = sub("=.*", "", arg)
            value = sub(".*=", "", arg)
            if (key != "") {
                args.list[[key]] = value
                next
            } else {
                fatal.log("get.cmd.args", "The {length(args.list) + 1} argument '{arg}' missing key")
            }
        } else {
            args.list[[arg]] = NA
        }
    }

    return(args.list)
}


#' nodoc
#' @export
process.cmd.args = function(required.args.name,
                            optional.args.default.value = list(),
                            copy.args = list()) {
    args = get.cmd.args()

    if ("help" %in% names(args)) {
        has.arg = FALSE

        if (length(required.args.name) > 0) {
            info.log("process.cmd.args", "Required args: {required.args.name}")
            has.arg = TRUE
        }

        if (length(optional.args.default.value) > 0) {
            info.log("process.cmd.args", "Optional args: {names(optional.args.default.value)} = {optional.args.default.value}")
            has.arg = TRUE
        }

        if (length(copy.args) > 0) {
            info.log("process.cmd.args", "Copy args: {names(copy.args)} = {copy.args}")
            has.arg = TRUE
        }

        if (!has.arg)
            info.log("process.cmd.args", "No arg")

        return(NULL)
    }

    for (arg.name in required.args.name) {
        if (is.null(args[[arg.name]]))
            fatal.log("process.cmd.args", "Missing arg: {arg.name}")
    }

    args = modifyList(optional.args.default.value, args)

    for (lhs.arg in names(copy.args)) {
        if (is.null(args[[lhs.arg]])) {
            rhs.arg = copy.args[[lhs.arg]]
            args[[lhs.arg]] = args[[rhs.arg]]
        }
    }

    return(args)
}
