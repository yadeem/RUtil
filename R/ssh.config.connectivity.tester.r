#' nodoc
#' @export
test.ssh.config = function(all.ssh.config.data, test.client = NULL, test.server = NULL) {
    info.log("test.ssh.config", "Start test ssh connectivity")
    info.log("test.ssh.config", "Test client: ", ifelse(is.null(test.client), "ALL", toString(test.client)))
    info.log("test.ssh.config", "Test server: ", ifelse(is.null(test.server), "ALL", toString(test.server)))

    success.connect = c()
    fail.connect = c()

    for (ssh.config.data in all.ssh.config.data) {
        user = ssh.config.data$user
        client = ssh.config.data$client
        if (!is.null(test.client) && (!client %in% test.client)) next

        server.config.data = ssh.config.data$server.config.data
        server = sapply(server.config.data, "[[", "Host")
        if (!is.null(test.server)) server = server[server %in% test.server]
        if (length(server) == 0) next
        host.user = paste0(user, "@", client)

        message("====================================")
        info.log("test.ssh.config", "Testing user: {host.user}")


        is.current.user = (client == get.host.name() && user == get.user.name())


        if (!is.current.user) {
            if (!check.enable.ssh.server(host.user)) {
                info.log("test.ssh.config", "Failed: ssh to host {host.user}")
                fail.connect = c(fail.connect, paste0(host.user, " => ", server))
                next
            }
            else {
                info.log("test.ssh.config", "Success: ssh to host {host.user}")
            }
        }

        for (svr in server) {
            info.log("test.ssh.config", "Connecting to server:{svr} ...")

            if (is.current.user) {
                success = check.enable.ssh.server(svr)
            } else {
                success = check.enable.ssh.server(host.user, svr)
            }

            if (success) {
                success.connect = c(success.connect, paste0(host.user, " => ", svr))
                info.log("test.ssh.config","Success")
            } else {
                fail.connect = c(fail.connect, paste0(host.user, " => ", svr))
                warning.log("test.ssh.config","Failed")
            }
        }
    }

    message("====================================")
    info.log("test.ssh.config", "Success: {length(success.connect)} Bad: {length(fail.connect)}")
    if (length(fail.connect) > 0) info.log("test.ssh.config", "Bad: {fail.connect}")
}