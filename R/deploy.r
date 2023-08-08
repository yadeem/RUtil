check.enable.ssh.server = function(server, next.server = NULL) {
    warn.opt = getOption("warn")
    options(warn = -1)
    on.exit(options(warn = warn.opt))

    host.exist = check.ssh.host.exist(server, next.server)
    host.key.check.args = ifelse(host.exist, "",  "-o 'StrictHostKeyChecking no' ")
    check.cmd = "ssh"
    if (!is.null(next.server)) {
        check.args = c("-q -o BatchMode=yes", server, shQuote(paste("ssh", host.key.check.args, next.server, "exit 0")))
        enable.cmd = "ssh"
        enable.args = c("-t", server, shQuote(paste("ssh-copy-id", next.server)))
    } else {
        check.args = c("-q -o BatchMode=yes", host.key.check.args, server, "exit 0")
        enable.cmd = "ssh-copy-id"
        enable.args = c(server)
    }

    success = system2(check.cmd, check.args, stdout = NULL, stderr = FALSE) == 0

    if (!success) {
        # do ssh-copy-id
        success = system2(enable.cmd, enable.args, stderr = FALSE) == 0
        if (!success) {
            # get fail info from std error
            result = system2(enable.cmd, enable.args, stderr = TRUE)
            if (grepl(".*WARNING: REMOTE HOST IDENTIFICATION HAS CHANGED!.*", result[5])) {
                # remove deprecated remote host in ~/.ssh/known_hosts
                success = remove.ssh.known.hosts(server, next.server)
            } else if (grepl("ssh-copy-id: ERROR: No identities found", result[1])) {
                need.key.client = ifelse(!is.null(next.server), server, "localhost")
                warning.log(
                    "check.enable.ssh.server",
                    "Please check if the ssh public key has been generated in the client: {need.key.client}"
                )
            }

            # redo ssh-copy-id
            if (success) {
                success = system2(enable.cmd, enable.args, stderr = FALSE) == 0
            }
        }
    }

    return(success)
}

get.known.host.name = function(server, next.server = NULL) {
    ssh.cmd = ifelse(is.null(next.server), "", paste("ssh", server))
    get.config.cmd = paste("ssh -G", ifelse(is.null(next.server), server, next.server))
    # get Host info from ~/.ssh/config
    config.info = system(paste(ssh.cmd, get.config.cmd), intern = T, ignore.stderr = TRUE)
    server.ip = sub("hostname ", "", config.info[2])
    server.port = sub("port ", "", config.info[3])
    server.name = ifelse(server.port == "22", server.ip, paste0("[", server.ip, "]:", server.port))
    return(server.name)
}

check.ssh.host.exist = function(server, next.server = NULL) {
    ssh.cmd = ifelse(is.null(next.server), "", paste("ssh", server))
    server.name = get.known.host.name(server, next.server)
    check.cmd = as.character(glue("{ssh.cmd} ssh-keygen -F '{server.name}'"))
    result = system(check.cmd,ignore.stdout = TRUE)
    return(result == 0)
}

remove.ssh.known.hosts = function(server, next.server = NULL) {
    ssh.cmd = ifelse(is.null(next.server), "", paste("ssh", server))
    server.name = get.known.host.name(server, next.server)
    known.host = ifelse(ssh.cmd != "", "'~/.ssh/known_hosts'", "~/.ssh/known_hosts")
    rm.args = paste0("ssh-keygen -f ", known.host, " -R ", server.name)
    success = system(paste(ssh.cmd, rm.args)) == 0

    # remove generated known_hosts.old file
    if (success) {
        known.host.old = ifelse(ssh.cmd != "", "'~/.ssh/known_hosts.old'", "~/.ssh/known_hosts.old")
        rm.old.file = system(paste(ssh.cmd, "rm", known.host.old))
    }
    return(success)
}

get.ssh.config.file = function(ssh.host.cmd, user) {
    user.home.cmd = paste0(ssh.host.cmd, " 'echo ~'", user)
    user.home = system(user.home.cmd, intern = T)
    return(file.path(user.home, ".ssh", "config"))
}

get.ping.config.file = function(ssh.host.cmd, user) {
    user.home.cmd = paste0(ssh.host.cmd, " 'echo ~'", user)
    user.home = system(user.home.cmd, intern = T)
    return(file.path(user.home, ".ssh", "ping.sh"))
}

check.host.file.exist = function(ssh.host.cmd, file) {
    warn.opt = getOption("warn")
    options(warn = -1)
    on.exit(options(warn = warn.opt))

    check.file.cmd = paste(ssh.host.cmd, "test -e", file)
    return(is.null(attributes(system(check.file.cmd, intern = T))))
}

compare.remote.file = function(ssh, host.user, remote.file, local.file) {
    scp = ifelse(ssh == "", "cp", "scp")
    remote.file = ifelse(ssh == "", remote.file, paste0(host.user, ":", remote.file))
    remote.copy.file = tempfile()
    download.cmd = paste(scp, remote.file, remote.copy.file)
    if (system(download.cmd, ignore.stdout = TRUE) != 0) {
        fatal.log("compare.remote.file", "Failed comparing remote file because copy error: {remote.file}")
    }
    compare.cmd = paste0("git diff --no-index -G. ", remote.copy.file, " ", local.file)
    ret = (system(compare.cmd) == 0)

    unlink(remote.copy.file)
    return(ret)
}

rsync.file = function(src, dest, chmod = NULL) {
    dest.dir = sub(basename(dest), "", sub(".*:", "", dest))
    mkdir.parameter = glue('--rsync-path="mkdir -p {dest.dir} && rsync"')
    exe.parameter = ifelse(is.null(chmod), "", paste0("--chmod=", chmod))
    cmd = paste("rsync -p", exe.parameter, mkdir.parameter, src, dest)
    info.log("rsync.file", cmd)
    ret = system(cmd, intern = T)
    success = length(ret) == 0 && is.null(attributes(ret))
    if (success) {
        info.log("rsync.file", "Success: {src} => {dest}")
    } else {
        warning.log("rsync.file", "Failed: {src} => {dest}")
    }
    return(success)
}

compare.file.by.git.diff = function(old, new, show.info = TRUE) {
    exit.code.option = ifelse(show.info, "", "--exit-code")
    compare.cmd = paste("git diff", exit.code.option, "--no-index -G.", old, new)
    return(system(compare.cmd) == 0)
}

parse.deploy.config.file = function(file.name) {
    xml.content = xmlParse(file.name)

    svr.group.node = getNodeSet(xml.content, "//ServerGroup")
    names(svr.group.node) = sapply(svr.group.node, \(x) xmlGetAttr(x, "name"))
    svr.group = lapply(svr.group.node, \(x) splitt.comma.value(xmlValue(x[["Server"]])))

    file.node = getNodeSet(xml.content, "//File")
    config = lapply(file.node, \(node) {
        v = list(
            src = node[["Src"]],
            dst = node[["Dst"]],
            server = node[["Server"]],
            type = node[["Type"]],
            chmod = node[["chmod"]]
        )

        v = (lapply(v, xmlValue))
        v = v[!is.na(v)]
        server = expand.host(splitt.comma.value(v$server), svr.group)
        if(length(server) == 0) return(list(v))
        ret = lapply(server, \(s) {
            v$server = s
            return(v)
        })
        return(ret)
    })
    config = unlist(config, recursive = F)
    return(config)
}

# ==================================================================


#' nodoc
#' @export
deploy.file = function(src, dest, confirm = TRUE, only.update.exist = FALSE, chmod = NULL, force = FALSE) {
    info.log("deploy.file", "Deploying: {src} => {dest}")

    if (length(src) == 0) {
        warning.log("deploy.file", "Failed: src is empty")
        return(FALSE)
    }

    if (length(dest) == 0) {
        warning.log("deploy.file", "Failed: dest is empty")
        return(FALSE)
    }

    # download dest file
    local.dest.file = tempfile(paste0(basename(dest), "_remote_"))
    on.exit(unlink(local.dest.file))

    download.cmd = paste("scp", dest, local.dest.file)
    success.downloaded = identical(system(download.cmd, ignore.stdout = TRUE, ignore.stderr = TRUE), 0L)

    if (only.update.exist && !success.downloaded) {
        info.log("deploy.file", "Skip: only deploy if the dest is exist: {dest}")
        return(FALSE)
    }


    system(paste("dos2unix", src))

    replace = TRUE
    # check diff between src and dest
    if (success.downloaded) {
        file.is.equal = compare.files(local.dest.file, src, verbose = confirm, mode = FALSE)
        if (!force && file.is.equal) {
            info.log("deploy.file", "Skip (identical): {src} => {dest}")
            return(TRUE)
        }
        check.info = ifelse(file.is.equal, "Force update file", "Replace the diff file above")
        if (confirm) {
            # confirm to deploy
            replace = confirm(check.info)
        } else {
            info.log("deploy.file", check.info)
        }
    } else {
        # create new file
        info.log("deploy.file", "The full file content:")
        message("---------------------------------")
        cat(paste0(readLines(src, warn = FALSE), "\n"))
        message("---------------------------------")
        if (confirm) {
            replace = confirm(glue("Create the file above: {dest}"))
        } else {
            info.log("deploy.file", "Create the file above: {dest}")
        }
    }

    # trans src file
    if (replace) {
        return(rsync.file(src, dest, chmod = chmod))
    } else {
        info.log("deploy", "Cancelled: {src} => {dest}")
        return(FALSE)
    }
}

#' nodoc
#' @export
deploy.crontab = function(src, user.at.server, confirm = TRUE) {
    if (is.null(user.at.server)) {
        ssh.cmd = ""
        user.at.server = "localhost"
    } else {
        ssh.cmd = glue("ssh {user.at.server}")
    }

    info.log("deploy.crontab", "Deploying: {src} => {user.at.server}")

    server.crontab = tempfile(paste0(user.at.server, "_crontab_"))
    download.cmd = paste(ssh.cmd, "crontab -l > ", server.crontab)
    download.success = system(download.cmd) == 0

    bashexec(glue("dos2unix {src}"))

    replace = TRUE
    if (download.success) {
        equal = compare.file.by.git.diff(server.crontab, src, show.info = confirm)
        if (equal) {
            info.log("deploy.crontab", "Skip (identical): {src} => {user.at.server}")
            return(TRUE)
        } else {
            if (confirm) {
                # confirm to deploy
                replace = confirm("Replace the diff file above")
            } else {
                info.log("deploy.crontab", "Replace the diff content above")
            }
        }
    } else {
        warning.log("deploy.crontab", "Cannot download crontab from {user.at.server}")
    }

    if (replace) {
        deploy.cmd = paste(ssh.cmd, "crontab <", src)
        info.log("deploy.crontab", "{deploy.cmd}")
        success = system(deploy.cmd) == 0
        if (success) {
            info.log("deploy.crontab", "Success: {src} => {user.at.server}")
        } else {
            warning.log("deploy.crontab", "Failed: {src} => {user.at.server}")
        }
        return(success)
    } else {
        info.log("deploy", "Canceled: {src} => {user.at.server}")
        return(FALSE)
    }
}

#' nodoc
#' @export
deploy = function(config.path, confirm = TRUE, force = FALSE, filter = NULL) {
    if (dir.exists(config.path)) {
        config.file = list.files(config.path, full.names = TRUE, recursive = TRUE)
    } else if (file.exists(config.path)) {
        config.file = config.path
    } else {
        warning.log("deploy", "Invalid config path: {config.path}")
    }

    for (file in config.file) {
        info.log("depoly", "Depolying config file: {file}")
        file.config = parse.deploy.config.file(file)
        for (i in seq_along(file.config)) {
            config = file.config[[i]]
            server = config$server
            dst = config$dst
            src = config$src
            type = config$type
            chmod = config$chmod

            if(!is.null(filter)) {
                if(!grepl(filter, basename(src))) next
            }

            debug.log("deploy", "#{i} config argument:
                Src: {src}
                Dst: {dst}
                Server: {server}
                Type: {type}
                chmod:{chmod}")

            if (identical(type, "crontab")) {
                deploy.crontab(src, server, confirm = confirm)
            } else {
                if (is.null(dst)) {
                    warning.log("deploy", "Failed: Dst is empty")
                    next
                }
                full.dst = ifelse(is.null(server), dst, paste0(server, ":", dst))
                executable = identical(type, "exe")
                deploy.file(src, full.dst, confirm = confirm, chmod = chmod, force = force)
            }
        }
    }
}
