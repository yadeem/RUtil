#' nodoc
#' @export
deploy.ssh.config.data = function(ssh.config.info, host.ssh.config.path, force.update.ping.script = FALSE) {
    for (i in 1:nrow(ssh.config.info)) {
        user = ssh.config.info$user[i]
        client = ssh.config.info$client[i]
        ssh.config.file = ssh.config.info$ssh.file[i]
        ping.config.file = ssh.config.info$ping.file[i]
        host.user = paste0(user, "@", client)

        message("====================================")
        info.log("deploy.ssh.config.data", "Deploying {host.user}")

        is.current.user = (client == get.host.name() && user == get.user.name())

        if (!is.current.user) {
            if (!check.enable.ssh.server(host.user)) {
                warning.log("deploy.ssh.config.data", "Failed: ssh to host {host.user}")
                next
            }
        }

        remote.host = if (is.current.user) NULL else paste0(host.user, ":")

        dest.host.ssh.config.file = paste0(remote.host, file.path(host.ssh.config.path, "config"))
        dest.ssh.file = paste0(remote.host, "~/.ssh/config")
        dest.ping.file = paste0(remote.host, "~/.ssh/ping.sh")

        suc = deploy.file(ssh.config.file, dest.ssh.file, chmod = "600")
        if (suc && length(dest.host.ssh.config.file) > 0) {
            deploy.file(ssh.config.file, dest.host.ssh.config.file, confirm = FALSE, only.update.exist = TRUE, chmod = "600")
        }
        if (suc || force.update.ping.script) {
            deploy.file(ping.config.file, dest.ping.file, confirm = FALSE)
        }
    }
}