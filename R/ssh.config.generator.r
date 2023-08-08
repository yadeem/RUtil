#' nodoc
#' @export
generate.ssh.config.data = function(ssh.xml.config.data) {

    server.config = ssh.xml.config.data$server.config
    client.config = ssh.xml.config.data$client.config
    access.config = ssh.xml.config.data$access.config
    user.config = ssh.xml.config.data$user.config
    server.order = names(ssh.xml.config.data$server.config)

    all.ssh.config.data = list()

    for (user in names(user.config)) {
        user.data = user.config[[user]]

        common.access.data = access.config$common.access.config
        user.access.data = access.config$user.access.config[[user]]

        if (is.null(user.access.data)) {
            available.access.data = common.access.data
        } else {
            available.access.data = modifyList(common.access.data, user.access.data)
        }

        for (i in 1:nrow(user.data)) {
            client = user.data$client[i]
            server = user.data$server[i]
            if (client == server) next
            clt.ssh.version = client.config[[client]]$ssh.version

            if (is.null(clt.ssh.version)) {
                warning.log("generate.ssh.config.data", "The client: {client} has no ssh version config")
            }

            client.access.data = available.access.data[[client]]
            data.list = list()

            while (!is.null(server)) {

                server.data = server.config[[server]]
                if (is.null(server.data)) {
                    fatal.log("generate.ssh.config.data", "Failed to find server config: {server}")
                }

                link.name = client.access.data[[server]][["link"]]
                has.access.link = !is.null(client.access.data[[server]]) && !is.null(link.name)

                if (has.access.link) {
                    host.data = server.data[["Link"]][[link.name]]
                    if(is.null(host.data)) {
                        fatal.log("generate.ssh.config.data", "The Access [client:{client} server:{server}] has invalid link: {link.name}")
                    }
                } else {
                    if (length(server.data[["Link"]]) > 1) {
                        fatal.log("generate.ssh.config.data", "The Access [client:{client} server:{server}] should set Link attribute")
                    }
                    if (length(server.data[["Link"]]) == 1) {
                        host.data = server.data[["Link"]][[1]]
                    } else {
                        host.data = server.data
                    }
                }

                data = list()
                data[["Host"]] = server
                data[["Hostname"]] = host.data$ip
                data[["Port"]] = host.data$port
                data[["User"]] = host.data$user

                server = host.data$proxy

                if (!identical(server, client)) {
                    data[["ProxyJump"]] = get.proxy.jump.value(clt.ssh.version, server)
                } else {
                    server = NULL
                }

                data.list[[length(data.list) + 1]] = data
            }

            data = list(user = user,
                        client = client,
                        server = user.data$server[i],
                        config.data = data.list)

            var = paste0(user, "@", client)
            all.ssh.config.data[[var]] = c(all.ssh.config.data[[var]], list(data))
        }
    }

    ret = list()
    # combine all server config of user@client
    for (user.client.ssh.config.data in all.ssh.config.data) {

        user = user.client.ssh.config.data[[1]]$user[1]
        client = user.client.ssh.config.data[[1]]$client[1]
        server.config.data = do.call("c", lapply(user.client.ssh.config.data, function(x) x$config.data))
        server = sapply(server.config.data, function(x) x$Host)
        names(server.config.data) = server
        server.config.data = server.config.data[order(match(server, server.order))]
        server.config.data = unique(server.config.data)
        ret[[paste0(user, "@", client)]] = list(user = user, client = client, server.config.data = server.config.data)
    }

    host.name = get.host.name()
    server.order = unique(c(host.name, server.order))
    ret = ret[order(match(sapply(ret, "[[", "client"), server.order))]

    return(ret)
}

get.proxy.jump.value = function(ssh.version, server) {
    if (is.null(server) || is.null(ssh.version)) return(server)

    a.b = as.numeric(unlist(str_split(ssh.version, "\\.")))

    equal.or.greater.than.8.8 = FALSE
    if (length(a.b) == 1) {
        equal.or.greater.than.8.8 = a.b > 8
    } else if (length(a.b) == 2) {
        equal.or.greater.than.8.8 = a.b[1] > 8 || (a.b[1] == 8 && a.b[2] >= 8)
    } else {
        fatal.log("get.proxy.jump.value", "Unsupport ssh version more than 2 split numbers: {ssh.version}")
    }

    return(ifelse(equal.or.greater.than.8.8, server, paste("-q", server)))
}

#' nodoc
#' @export
generate.ssh.config.file = function(all.ssh.config.data, output.path) {

    ret = data.table()

    for (config.data in all.ssh.config.data) {

        server.config.data = config.data$server.config.data
        user = config.data$user
        client = config.data$client

        info.log("generate.ssh.config.file", "Generate: {user}@{client} ssh Config file")

        ssh.file = gen.ssh.config.file(user, client, server.config.data)
        ping.file = gen.ping.config.file(user, client, server.config.data)
        ret = rbind(ret, data.table(user, client, ssh.file, ping.file))
    }

    return(ret)
}

gen.ssh.config.file = function(user, client, server.config.data){
    config.str = c()
    for (config.data in server.config.data) {
        prefix = names(config.data)
        host.string = paste0(prefix, " ", config.data)
        host.string[2:length(host.string)] = paste0("    ", host.string[-1])
        host.string = c(host.string, "") # add a blank line between hosts
        config.str = c(config.str, host.string)
    }

    if(length(config.str) == 0) return(NULL)
    ssh.file.name = paste0(user, "@", client, ".sshconfig")
    output.file = file.path(output.path, ssh.file.name)
    write(config.str, output.file)
    return(output.file)
}

gen.ping.config.file = function(user, client, server.config.data){
    config.str = c()
    for (config.data in server.config.data) {
        if(!is.null(config.data$ProxyJump)) next # only ping direct host
        host = config.data$Host
        host.ip = config.data$Hostname
        ping.str = paste0("ping -i 60 ", host.ip, " | while read pong; do echo \"$(date): $pong\"; done &>> ~/.ssh/log/", host, ".log", " &")
        config.str = c(config.str, ping.str)
    }
    if(length(config.str) == 0) return(NULL)
    config.str = c("mkdir -p ~/.ssh/log", config.str)
    ping.file.name = paste0(user, "@", client, ".pingscript")
    output.file = file.path(output.path, ping.file.name)
    write(config.str, output.file)
    return(output.file)
}