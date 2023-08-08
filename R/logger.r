is.rutil.logger.inited = function() {
    return(exists(".rutil.logger.initialized", envir = .GlobalEnv, inherits = FALSE))
}

is.rutil.logger.path.inited = function() {
    return(exists(".rutil.logger.path", envir = .GlobalEnv, inherits = FALSE))
}

get.rutil.logger.path = function() {
    if(is.rutil.logger.path.inited()) .rutil.logger.path else NULL
}

################################################################################

#' nodoc
#' @export
gen.log.path = function(log.root, name, tz = NULL) {
    time = Sys.time()
    if (!is.null(tz)) setattr(time, "tzone", tz)
    date = format(time, format = "%Y-%m-%d")

    date.path = file.path(log.root, date)
    if (!dir.exists(date.path)) dir.create(date.path, FALSE, TRUE)
    exist.log = list.files(date.path, pattern = paste0(name, "[0-9_]*", ".log"))
    new.log.suffix = ifelse(length(exist.log) > 0, glue("_{length(exist.log)}"), "")
    ret = file.path(date.path, glue("{name}{new.log.suffix}.log"))
    ret = normalizePath(ret, mustWork = FALSE)
    return(ret)
}


colorized.by.log.level = function(level, msg) {
    fail_on_missing_package('crayon')
    color <- switch(
        attr(level, 'level'),
        'FATAL'   = crayon::combine_styles(crayon::bold, crayon::make_style('red1')),
        'ERROR'   = crayon::make_style('red4'),
        'WARN'    = crayon::make_style('darkorange'),
        'INFO'    = crayon::reset,
        'DEBUG'   = crayon::reset,
        'TRACE'   = crayon::reset,
        stop('Unknown log level')
    )
    paste0(color(msg), crayon::reset(''))
}

#' nodoc
#' @export
init.logger = function(log.path = "", threshold = INFO) {

    if (is.rutil.logger.inited() && log.path == "" && threshold == INFO) {
        return(invisible())
    }

    if (log.path != "" && is.rutil.logger.path.inited()) {
        warning("Cannot reinitialize the log path that already exsits:", .rutil.logger.path)
        return(invisible())
    }

    # TODO config the log output file, layout, level
    if (!requireNamespace("logger", quietly = TRUE)) {
        stop("Missing logger pacakge")
    }

    log.formatter = "{RUtil:::colorized.by.log.level(levelr, paste0(
        {format(time, \"%Y-%m-%d %H:%M:%OS3\")},'|',
        {toTitleCase(tolower(level))},'|',
        {namespace},'|',
        {msg}))}"

    layout = layout_glue_generator(format = log.formatter)
    log_threshold(threshold)
    log_layout(layout)

    if (log.path != "") {
        log_appender(appender_tee(log.path))
        .rutil.logger.path <<- log.path
        options(error = function() {
            error.log("options.error", geterrmessage())
            if (!is.null(get.this.script.path())) {
                q(status = 1)
            }
        })
    }

    .rutil.logger.initialized <<- TRUE
}


#' nodoc
#' @export
trace.log = function(logger.name, ...) {
    init.logger()
    log_level(TRACE, ..., namespace = logger.name, .topenv = parent.frame(), .null = "")
}

#' nodoc
#' @export
debug.log = function(logger.name, ...) {
    init.logger()
    log_level(DEBUG, ..., namespace = logger.name, .topenv = parent.frame(), .null = "")
}

#' nodoc
#' @export
info.log = function(logger.name, ...) {
    init.logger()
    log_level(INFO, ..., namespace = logger.name, .topenv = parent.frame(), .null = "")
}

#' nodoc
#' @export
warning.log = function(logger.name, ...) {
    init.logger()
    log_level(WARN, ..., namespace = logger.name, .topenv = parent.frame(), .null = "")
}

#' nodoc
#' @export
error.log = function(logger.name, ...) {
    init.logger()
    log_level(ERROR, ..., namespace = logger.name, .topenv = parent.frame(), .null = "")
}

#' nodoc
#' @export
fatal.log = function(logger.name, ..., stack.trace.nskip = 1) {
    init.logger()
    log_level(FATAL, ..., namespace = logger.name, .topenv = parent.frame(), .null = "")
    printt.stack.trace(stack.trace.nskip, print.stack.variable = FALSE)
    stop()
}

#' nodoc
#' @export
progress.print = function(percent, ...) {
    init.logger()
    if (log_threshold() >= logger::INFO) {
        cat("\r", format(percent * 100, digits = 4), "% ", ...)
        if (percent == 1) cat("\n")
    }
}