#' nodoc
#' @export
create.file = function(path, create.dir = TRUE) {
    dir = dirname(path)
    has.dir = dir.exists(dir)
    if (!has.dir && !create.dir) return(FALSE)
    if (!has.dir) dir.create(dir)
    if (file.exists(path)) return(TRUE)
    file.create(path)
    return(TRUE)
}

check.file.is.writing = function(host, file.path) {
    warn.opt = getOption("warn")
    options(warn = -1)
    fuser.cmd = paste(host, "fuser -v", file.path, "2>&1")
    ret = system(fuser.cmd, intern = TRUE)
    if (length(ret) == 0) return(FALSE)
    return(any(grepl("F....", ret)))
    options(warn = warn.opt)
}

#' nodoc
#' @export
wait.for.file = function(path, host = NULL, check.interval = 5, condition = "creation", timeout = NULL) {
    prev.file.size = NULL

    host = ifelse(!is.null(host), paste("ssh", host), "")
    stat.cmd = paste(host, "stat", path)

    start.time = Sys.time()
    while (T) {


        now = Sys.time()
        if (!is.null(timeout) &&
            difftime(now, start.time, units = "secs") > timeout) {
            info.log("wait.for.file", "Timeout was reached after {timeout} seconds: wait for {host}:{path}")
            return(FALSE)
        }

        info.log("wait.for.file", "Waiting for {condition} {host}:{path}")

        warn.opt = getOption("warn")
        options(warn = -1)
        rsp = paste(system(stat.cmd, intern = TRUE, ignore.stderr = TRUE), collapse = " ")
        options(warn = warn.opt)

        size = regmatches(rsp, regexpr("Size: [[:digit:]]+", rsp))

        if (length(size) == 0) {
            info.log("wait.for.file", "File does not exist")
            Sys.sleep(check.interval)
            next
        }

        if (condition == "creation") break

        file.is.writing = check.file.is.writing(host, path)

        if (condition == "stable" && !file.is.writing) {
            if (!is.null(prev.file.size) && prev.file.size == size) break
            prev.file.size = size
            info.log("wait.for.file", "File size changed to {size}")
        } else if (condition == "modification" && !file.is.writing) {
            if (!is.null(prev.file.size) && prev.file.size != size) break
            prev.file.size = size
            info.log("wait.for.file", "File size does not change")
        } else {
            info.log("wait.for.file", "File is writing")
        }
        Sys.sleep(check.interval)
    }

    info.log("wait.for.file", "End waiting for {host}:{path}")
    return(TRUE)
}

#' nodoc
#' @export
create.signal.file = function(signal, date = NULL, signal.root = "/home/dba/signal") {
    if (is.null(date)) {
        signal.file = glue("{signal}.sig")
    } else {
        signal.file = glue("{date}.{signal}.sig")
    }
    signal.file = file.path(signal.root, signal.file)
    create.file(signal.file)
}

#' nodoc
#' @export
wait.for.signal.file = function(signal, date = NULL, timeout = NULL, signal.root = "/home/dba/signal") {
    if (is.null(date)) {
        signal.file = glue("{signal}.sig")
    } else {
        signal.file = glue("{date}.{signal}.sig")
    }
    signal.file = file.path(signal.root, signal.file)
    return(wait.for.file(signal.file, timeout = timeout))
}