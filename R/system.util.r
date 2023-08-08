#'@export
is.windows = function() {
    return(Sys.info()["sysname"] == "Windows")
}

#' nodoc
#' @export
is.linux = function() {
    return(Sys.info()["sysname"] == "Linux")
}

#' nodoc
#' @export
get.host.desc = function() {
    info = Sys.info()
    desc = paste0(info["nodename"], " ", info["sysname"], " ", info["release"], " ", info["version"])
    return(desc)
}

#' nodoc
#' @export
get.host.name = function() {
    host.name = Sys.info()["nodename"]
    names(host.name) = NULL
    return(host.name)
}

#' nodoc
#' @export
get.user.name = function() {
    user.name = Sys.info()["user"]
    names(user.name) = NULL
    return(user.name)
}

get.local.ip.win = function() {
    str = system("ipconfig", intern = TRUE)
    ipstr = str[grep("IPv4", str)]
    ip = gsub(".*? ([[:digit:]])", "\\1", ipstr)
    lo.ip = which(ip == "127.0.0.1")
    if (length(lo.ip)) ip = ip[-lo.ip]
    return(ip)
}

get.local.ip.linux = function() {
    str = system("ip addr", intern = TRUE)
    ipstr = str[grep("inet ", str)]
    ip = gsub(".*inet ([^ ]+)/.*", "\\1", ipstr)
    lo.ip = which(ip == "127.0.0.1")
    if (length(lo.ip)) ip = ip[-lo.ip]
    return(ip)
}

#' nodoc
#' @export
get.local.ip = function() {
    if (is.linux()) {
        return(get.local.ip.linux())
    } else {
        return(get.local.ip.win())
    }
}

#' nodoc
#' @export
get.subnet.ip = function(ip) {
    return(gsub("\\.\\d+$", "", ip))
}

#' doc
#' @export
get.this.script.path = function() {
    for (i in sys.nframe():1) {
        if (identical(sys.function(-i), base::source)) {
            this.script.path = normalizePath(sys.frame(-i)$ofile)
            if (!is.null(this.script.path)) return(this.script.path)
        }
    }

    cmd.args = commandArgs(trailingOnly = FALSE)
    cmd.args.trailing = commandArgs(trailingOnly = TRUE)
    cmd.args.leading = cmd.args[1:(length(cmd.args) - length(cmd.args.trailing))]

    pattern = "^--file="
    file.arg = cmd.args.leading[grepl(pattern, cmd.args.leading)]
    this.script.path = gsub(pattern, "", file.arg)
    if (length(this.script.path) > 0) return(file_path_as_absolute(this.script.path))

    return(NULL)
}

#' doc
#' @export
source.relative = function(file, ...) {
    script.path = get.this.script.path()
    if (is.null(script.path)) {
        script.path = getwd()
    } else {
        script.path = dirname(script.path)
    }

    path = file.path(script.path, file)
    source(path, ...)
}

#' doc
#' @export
bashexec = function(cmd, log.file = get.rutil.logger.path(), dry.run = FALSE) {

    if (dry.run) {
        info.log("bashexec", cmd)
        return(TRUE)
    }

    if (!is.null(log.file)) {
        create.file(log.file)
        cmd = paste(cmd, "2>&1 | tee -a", shQuote(log.file))
    }

    cmd = paste(cmd, "; exit ${PIPESTATUS[0]}")
    return(system(cmd, intern = FALSE) == 0)
}

#' doc
#' @export
remote.exec = function(server, cmd, dry.run = FALSE) {

    server.cmd = glue('ssh {server} "{cmd}"')

    for(svr.cmd in server.cmd) {
        info.log("remote.exec", svr.cmd)
        if (dry.run) next
        system(svr.cmd)
    }
}